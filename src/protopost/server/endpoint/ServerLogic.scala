package protopost.server.endpoint

import zio.*

import sttp.model.StatusCode
import sttp.model.headers.{CookieValueWithMeta,Cookie}
import Cookie.SameSite
import sttp.tapir.files.*
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.common.api.{*,given}
import protopost.common.{EmailAddress,Password,PosterId}
import protopost.server.{AppResources,ExternalConfig}
import protopost.server.LoggingApi.*
import protopost.server.exception.{ApparentBug,BadPostDefinition,MissingConfig,ResourceNotFound,UnknownPost}

import protopost.server.db.PgDatabase

import com.mchange.rehash.*

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.mchange.sc.zsqlutil.*
import protopost.server.exception.{BadCookieSettings,BadCredentials,InsufficientPermissions,NotLoggedIn}
import protopost.server.jwt
import sttp.model.headers.CookieValueWithMeta

object ServerLogic extends SelfLogging:

  private val service = protopost.common.Service.protopost // forseeing abstracting some of this to a more abstract restack library

  private val JtiEntropyBytes = 16

  private def newJti( appResources : AppResources ) : String =
    import com.mchange.cryptoutil.{*,given}
    val arr = Array.ofDim[Byte](JtiEntropyBytes)
    appResources.entropy.nextBytes(arr)
    arr.base64url

  private def toEpochSecond( instant : Instant ) = instant.toEpochMilli / 1000

  def authenticatePoster(appResources : AppResources)(authInfo : jwt.PosterAuthInfo): ZOut[jwt.AuthenticatedPoster] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity

        // Try high security token first, then fall back to low security token
        val mbTokenLevel =
          val hiTup = authInfo.highSecurityToken.map( token => (token, jwt.SecurityLevel.high) )
          def lowTup = authInfo.lowSecurityToken.map( token => (token, jwt.SecurityLevel.low) )
          hiTup orElse lowTup

        mbTokenLevel match
          case Some(Tuple2(token,level)) =>
            try
              val claims = jwt.decodeVerifyJwt(identity.publicKey)(jwt.Jwt(token))

              // Check if token is expired
              val now = Instant.now()
              if claims.expiration.isBefore(now) then
                throw new BadCredentials("JWT token has expired")

              // verify claimed security level
              if level != claims.securityLevel then
                throw new BadCredentials( s"Token authenticated as security level ${claims.securityLevel} was misplaced in request as token_security_+${level}" )

              jwt.AuthenticatedPoster(claims, level)
            catch
              case bc: BadCredentials => throw bc
              case t: Throwable =>
                WARNING.log("Could not validate JWT token during authentication.", t)
                throw new BadCredentials("Invalid JWT token", t)
          case None =>
            throw new NotLoggedIn("No authentication token provided")

  def parseSubject( aposter : jwt.AuthenticatedPoster ) : Subject = Subject.parse(aposter.claims.subject)

  def posterInfo( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( unit : Unit ) : ZOut[PosterNoAuth] =
    ZOut.fromOptionalTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.posterById(subject.posterId)(conn).map( _.toApiPosterNoAuth )

  def destinations( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( unit : Unit ) : ZOut[Set[Destination]] =
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.destinationsByPosterId(subject.posterId)(conn)

  def newPost( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postDefinitionCreate : PostDefinitionCreate ) : ZOut[PostDefinition] =
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      if postDefinitionCreate.owner != subject.posterId then
        ZIO.fail( new InsufficientPermissions(s"Poster #{subject.posterId} can not create a post that will be owned by specified owner #${postDefinitionCreate.owner}") )
      else
        withConnectionTransactional(ds): conn =>
          val postId = db.newPost(
            destinationSeismicNodeId = postDefinitionCreate.destinationSeismicNodeId,
            destinationName          = postDefinitionCreate.destinationName,
            owner                    = postDefinitionCreate.owner,
            postAnchor               = postDefinitionCreate.postAnchor,
            sprout                   = postDefinitionCreate.sprout,
            inReplyToHref            = postDefinitionCreate.inReplyToHref,
            inReplyToMimeType        = postDefinitionCreate.inReplyToMimeType,
            inReplyToGuid            = postDefinitionCreate.inReplyToGuid,
            authors                  = postDefinitionCreate.authors
          )( conn )
          db.postDefinitionForId( postId )( conn ).getOrElse:
            throw new ApparentBug( s"We created a new post in the database with id #${postId}, yet when we look it up there is no post?" )

  def destinationPosts( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( destinationIdentifier : DestinationIdentifier ) : ZOut[Set[PostDefinition]] =
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      //val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.postDefinitionsForDestination(destinationIdentifier.seismicNodeId, destinationIdentifier.name)(conn)

  private def translateNullableUpdateValueToDb[T]( existingValue : Option[T], updater : UpdateValue[T] ) : Option[T] =
    import UpdateValue.*
    updater match
      case `update`( t ) => Some(t)
      case `set-to-none` => None
      case `leave-alone` => existingValue

  private def translateSeqUpdateValueToDb[T]( existingValue : Seq[T], updater : UpdateValue[Seq[T]] ) : Seq[T] =
    import UpdateValue.*
    updater match
      case `update`( seq ) => seq
      case `set-to-none` => Seq.empty
      case `leave-alone` => existingValue

  def updatePostDefinition( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( pdu : PostDefinitionUpdate ) : ZOut[PostDefinition] =
    // println( pdu )
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )

      withConnectionTransactional(ds): conn =>
        db.postDefinitionForId( pdu.postId )( conn ) match
          case Some( currentPostDefinition ) =>
            if currentPostDefinition.owner.id == subject.posterId then
              val postId            : Int             = pdu.postId
              val title             : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.title, pdu.title )
              val postAnchor        : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.postAnchor, pdu.postAnchor )
              val sprout            : Option[Boolean] = translateNullableUpdateValueToDb(currentPostDefinition.sprout, pdu.sprout )
              val inReplyToHref     : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.inReplyToHref, pdu.inReplyToHref )
              val inReplyToMimeType : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.inReplyToMimeType, pdu.inReplyToMimeType )
              val inReplyToGuid     : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.inReplyToGuid, pdu.inReplyToGuid )

              db.updatePostDefinitionMain(
                postId = postId,
                title = title,
                postAnchor = postAnchor,
                sprout = sprout,
                inReplyToHref = inReplyToHref,
                inReplyToMimeType = inReplyToMimeType,
                inReplyToGuid = inReplyToGuid
              )( conn )

              val updateAuthors = pdu.authors != UpdateValue.`leave-alone`

              if updateAuthors then
                val newSeq = translateSeqUpdateValueToDb( currentPostDefinition.authors, pdu.authors )
                db.replaceAuthorsForPost( postId, newSeq )( conn ) // we know authors is nonEmpty if updateAuthors is true

              db.postDefinitionForId( pdu.postId )( conn ).getOrElse:
                throw new ApparentBug( s"In same transaction as a successful update of post with id ${postId}, the post no longer exists?!?" )
            else
              throw new InsufficientPermissions( s"User ${subject.posterId} cannot update a post owned by user ${currentPostDefinition.owner.id}" )
          case None =>
            throw new UnknownPost( s"No post with is ${pdu.postId} exists to update." )

  def jwks( appResources : AppResources )(u : Unit) : ZOut[jwt.Jwks] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity
        val jwk = jwt.Jwk( identity.publicKey, identity.service )
        jwt.Jwks( List( jwk ) )

  def client( appResources : AppResources )(unit : Unit) : ZOut[String] =
    ZOut.fromTask( ZIO.attempt(protopost.client.client_top_html( appResources.localIdentity.location.toUrl ).text) )

  extension ( pwd : protopost.common.Password )
    def toRehash : com.mchange.rehash.Password = com.mchange.rehash.Password(Password.s(pwd))

  private def peel( either : Either[String,CookieValueWithMeta] ) =
    either match
      case Left( str )   => throw new BadCookieSettings( str )
      case Right( cvwm ) => cvwm

  def login( appResources : AppResources )( emailPassword : EmailPassword ) : ZOut[(CookieValueWithMeta, CookieValueWithMeta, LoginStatus)] =
    import com.mchange.rehash.str

    val email = emailPassword.email
    val password = emailPassword.password
    val database = appResources.database
    val identity = appResources.localIdentity

    val checkCredentials =
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        import VerificationResult.*
        database.posterForEmail( email )( conn ) match
          case Some( pwa ) =>
            val fetchHash : PosterId => Option[BCryptHash] = posterId => database.fetchHashForPoster( posterId )( conn )
            val storeHash : ( PosterId, BCryptHash ) => Unit = ( posterId : PosterId, hash : BCryptHash ) => database.updateHashForPoster( posterId, hash )( conn )
            appResources.authManager.verifyRehash( pwa.id, password.toRehash, fetchHash, storeHash ) match
              case OK => ZIO.succeed( pwa.id )
              case WrongPassword => ZIO.fail( new BadCredentials( "The password given fails to validate." ) )
              case UserNotFound => ZIO.fail( new BadCredentials( s"No poster found with id '${pwa.id}', despite identification of this poster by email ${email}?!?" ) )
          case None =>
            ZIO.fail( new BadCredentials( s"No poster found with e-mail '${emailPassword.email}'." ) )
    def issueTokens( posterId : PosterId ) : Task[(CookieValueWithMeta,CookieValueWithMeta,LoginStatus)] =
      ZIO.attempt:
        val subject = Subject(posterId,email)
        val issuedAt = Instant.now()
        val highSecurityMinutes
          = appResources.externalConfig
              .get( ExternalConfig.Key.`protopost.token.security.high.validity.minutes` )
              .map( _.toInt )
            .getOrElse( throw new MissingConfig( ExternalConfig.Key.`protopost.token.security.high.validity.minutes`.toString ) )
        val lowSecurityMinutes
          = appResources.externalConfig
              .get( ExternalConfig.Key.`protopost.token.security.low.validity.minutes` )
              .map( _.toInt )
            .getOrElse( throw new MissingConfig( ExternalConfig.Key.`protopost.token.security.low.validity.minutes`.toString ) )
        val highSecurityExpiration = issuedAt.plus( highSecurityMinutes, ChronoUnit.MINUTES )
        val lowSecurityExpiration = issuedAt.plus( lowSecurityMinutes, ChronoUnit.MINUTES )
        val highSecurityJti = newJti(appResources) // for eventual implementation of token revocation
        val lowSecurityJti = newJti(appResources)  // for eventual implementation of token revocation
        val highSecurityJwt = jwt.createSignJwt( appResources.localIdentity.privateKey )(
          keyId = service.toString,
          subject = subject.toString(),
          issuedAt = issuedAt,
          expiration = highSecurityExpiration,
          jti = highSecurityJti,
          securityLevel = jwt.SecurityLevel.high
        )
        val lowSecurityJwt = jwt.createSignJwt( appResources.localIdentity.privateKey )(
          keyId = service.toString,
          subject = subject.toString(),
          issuedAt = issuedAt,
          expiration = lowSecurityExpiration,
          jti = lowSecurityJti,
          securityLevel = jwt.SecurityLevel.low
        )
        val highSecurityCookieValue = CookieValueWithMeta.safeApply(jwt.Jwt.s(highSecurityJwt), expires=Some(highSecurityExpiration), secure=appResources.inProduction, httpOnly=true, sameSite=Some(SameSite.Strict))
        val lowSecurityCookieValue  = CookieValueWithMeta.safeApply(jwt.Jwt.s(lowSecurityJwt),  expires=Some(lowSecurityExpiration),  secure=appResources.inProduction, httpOnly=true, sameSite=Some(SameSite.Strict))


        ( peel(highSecurityCookieValue), peel(lowSecurityCookieValue), loginStatusFromExpirations(highSecurityExpiration, lowSecurityExpiration) )

    ZOut.fromTask:
      checkCredentials.flatMap( issueTokens )

  def logout( appResources : AppResources )( unit : Unit ) : ZOut[(CookieValueWithMeta, CookieValueWithMeta, LoginStatus)] =
    ZOut.fromTask:
      ZIO.attempt:
        val highSecurityCookieValue = CookieValueWithMeta.safeApply("irrelevant", expires=Some(Instant.EPOCH), secure=appResources.inProduction, httpOnly=true, sameSite=Some(SameSite.Strict))
        val lowSecurityCookieValue  = CookieValueWithMeta.safeApply("irrelevant", expires=Some(Instant.EPOCH), secure=appResources.inProduction, httpOnly=true, sameSite=Some(SameSite.Strict))
        (peel(highSecurityCookieValue), peel(lowSecurityCookieValue), protopost.common.api.LoginStatus.empty)

  private def loginStatusFromExpirations( highSecurityExpiration : Instant, lowSecurityExpiration : Instant ) : protopost.common.api.LoginStatus =
    val nowEpochSecond = java.lang.System.currentTimeMillis() / 1000
    val highSecuritySecondsRemaining = math.max(toEpochSecond(highSecurityExpiration) - nowEpochSecond, 0L)
    val lowSecuritySecondsRemaining = math.max(toEpochSecond(lowSecurityExpiration) - nowEpochSecond, 0L)
    protopost.common.api.LoginStatus( highSecuritySecondsRemaining, lowSecuritySecondsRemaining )

  def loginStatus( appResources : AppResources )( highSecurityToken : Option[String], lowSecurityToken : Option[String] ) : ZOut[LoginStatus] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity

        // absent a time machine, expiry at the UNIX epoch date means
        // a token has already expired
        def extractExpirationOrEpoch( tokenOpt : Option[String] ) : Instant =
          tokenOpt match
            case Some( tokenStr ) =>
              try
                val verified = jwt.decodeVerifyJwt(identity.publicKey)(jwt.Jwt(tokenStr))
                verified.expiration
              catch
                case t : Throwable =>
                  WARNING.log("Could not validate a JWT.", t)
                  Instant.EPOCH
            case None =>
                  Instant.EPOCH

        val highSecurityExpiration = extractExpirationOrEpoch(highSecurityToken)
        val lowSecurityExpiration = extractExpirationOrEpoch(lowSecurityToken)
        loginStatusFromExpirations( highSecurityExpiration, lowSecurityExpiration )

  def newDraft( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( npr : NewPostRevision ) : ZOut[Option[PostRevisionIdentifier]] =
    ZOut.fromTask:
      val db = appResources.database
      withConnectionTransactional( appResources.dataSource ): conn =>
        val subject = parseSubject( authenticatedPoster )
        val mbPostDefinition = db.postDefinitionForId(npr.postId)( conn )
        mbPostDefinition match
          case Some( postDefinition ) =>
            if postDefinition.owner.id != subject.posterId then
              throw new InsufficientPermissions( s"The logged-in subject '${subject}' does not own post with ID ${npr.postId}" )
            else
              val mbSaveTime = db.newPostRevision(npr.postId,npr.contentType,npr.body)( conn )
              mbSaveTime.map( saveTime => PostRevisionIdentifier(npr.postId, saveTime) )
          case None =>
            throw new ResourceNotFound(s"No post with ID ${npr.postId} was found!")

  def latestDraft( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int ) : ZOut[Option[RetrievedPostRevision]] =
    ZOut.fromTask:
      val db = appResources.database
      withConnectionTransactional( appResources.dataSource ): conn =>
        val subject = parseSubject( authenticatedPoster )
        val mbPostDefinition = db.postDefinitionForId(postId)( conn )
        mbPostDefinition match
          case Some( postDefinition ) =>
            if postDefinition.owner.id != subject.posterId then
              throw new InsufficientPermissions( s"The logged-in subject '${subject}' does not own post with ID ${postId}" )
            else
              db.postRevisionLatest(postId)( conn )
          case None =>
            throw new ResourceNotFound(s"No post with ID ${postId} was found!")


end ServerLogic
