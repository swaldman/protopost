package protopost.server.endpoint

import zio.*

import sttp.model.StatusCode
import sttp.model.headers.{CookieValueWithMeta,Cookie}
import Cookie.SameSite
import sttp.tapir.files.*
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*
import sttp.model.headers.CookieValueWithMeta

import protopost.common.api.{*,given}
import protopost.common.{EmailAddress,Password,PosterId}
import protopost.server.{AppResources,ExternalConfig}
import protopost.server.LoggingApi.*
import protopost.server.exception.*
import protopost.server.jwt
import protopost.server.rss
import protopost.server.db.PgDatabase

import com.mchange.rehash.*

import java.io.{BufferedInputStream,InputStream}
import java.sql.Connection
import javax.sql.DataSource
import java.time.Instant
import java.time.temporal.ChronoUnit

import scala.util.control.NonFatal

import com.mchange.sc.zsqlutil.*
import com.mchange.conveniences.string.*

import zio.stream.*
import scala.util.Using
import java.net.URLConnection

import com.mchange.mailutil.Smtp

object ServerLogic extends SelfLogging:

  private val service = protopost.common.Service.protopost // forseeing abstracting some of this to a more abstract restack library

  /*
   *  Constants
   */

  private val JtiEntropyBytes = 16

  private val GoodPathElementRegex = """^[A-Za-z0-9\.\-\_]+$""".r
  private val K32 = 32 * 1024

  /*
   *  Small utilities
   */

  private def newJti( appResources : AppResources ) : String =
    import com.mchange.cryptoutil.{*,given}
    val arr = Array.ofDim[Byte](JtiEntropyBytes)
    appResources.entropy.nextBytes(arr)
    arr.base64url

  private def toEpochSecond( instant : Instant ) = instant.toEpochMilli / 1000

  /*
   *  Utilities for pre-authenticated server logic
   */

  private def loginStatusFromExpirations( highSecurityExpiration : Instant, lowSecurityExpiration : Instant ) : protopost.common.api.LoginStatus =
    val nowEpochSecond = java.lang.System.currentTimeMillis() / 1000
    val highSecuritySecondsRemaining = math.max(toEpochSecond(highSecurityExpiration) - nowEpochSecond, 0L)
    val lowSecuritySecondsRemaining = math.max(toEpochSecond(lowSecurityExpiration) - nowEpochSecond, 0L)
    protopost.common.api.LoginStatus( highSecuritySecondsRemaining, lowSecuritySecondsRemaining )

  private def peel( either : Either[String,CookieValueWithMeta] ) =
    either match
      case Left( str )   => throw new BadCookieSettings( str )
      case Right( cvwm ) => cvwm

  extension ( pwd : protopost.common.Password )
    def toRehash : com.mchange.rehash.Password = com.mchange.rehash.Password(Password.s(pwd))


  /*
   *  Non-authenticated endpoint server logic
   */

  def jwks( appResources : AppResources )(u : Unit) : ZOut[jwt.Jwks] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity
        val jwk = jwt.Jwk( identity.publicKey, identity.service )
        jwt.Jwks( List( jwk ) )

  def client( appResources : AppResources )(unit : Unit) : ZOut[String] =
    ZOut.fromTask( ZIO.attempt(protopost.client.client_top_html( appResources.localIdentity.location.toUrl ).text) )

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

  def optionalFeatures( appResources : AppResources )( unit : Unit ) : ZOut[Set[OptionalFeature]] =
    ZOut.fromTask:
      ZIO.attempt:
        Set.from(appResources.optionalMailConfig.map( _ => OptionalFeature.Smtp )) +
          OptionalFeature.Preview + // for now...
          OptionalFeature.Always    // for ever...

  /*
   *  Authentication server logic
   */

  def authenticatePoster(appResources : AppResources)(authInfo : jwt.PosterAuthInfo): ZOut[AuthenticatedPosterMbJwtExpiration] =
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

              val extensionTokenMbJwtExpiration =
                if level == jwt.SecurityLevel.low && claims.expiration.isBefore( now.plus(1, ChronoUnit.HOURS) ) then // if our low-security login is expiring within an hour
                  val expiration = now.plus(2, ChronoUnit.HOURS)
                  val extensionLowSecurityJwt = jwt.createSignJwt( appResources.localIdentity.privateKey )(
                    keyId = claims.keyId,
                    subject = claims.subject,
                    issuedAt = now,
                    expiration = expiration,
                    jti = newJti(appResources),
                    securityLevel = jwt.SecurityLevel.low
                  )
                  Some( (jwt = extensionLowSecurityJwt, expiration = expiration) )
                else
                  None

              val authenticatedPoster = jwt.AuthenticatedPoster(claims, level)
              ( authenticatedPoster = authenticatedPoster, mbJwtExpiration = extensionTokenMbJwtExpiration )
            catch
              case bc: BadCredentials => throw bc
              case t: Throwable =>
                WARNING.log("Could not validate JWT token during authentication.", t)
                throw new BadCredentials("Invalid JWT token", t)
          case None =>
            throw new NotLoggedIn("No authentication token provided")

  /*
   *  Utilities for authenticated server logic
   */

  private def parseSubject( aposter : jwt.AuthenticatedPoster ) : Subject = Subject.parse(aposter.claims.subject)

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

  private def opIfOwner[T]( db : PgDatabase, authenticatedPoster : jwt.AuthenticatedPoster, conn : Connection, postId : Int )( op : (PgDatabase,Connection) => T) : T =
    val subject = parseSubject( authenticatedPoster )
    val mbPostDefinition = db.postDefinitionForId(postId)( conn )
    mbPostDefinition match
      case Some( postDefinition ) =>
        if postDefinition.owner.id != subject.posterId then
          throw new InsufficientPermissions( s"The logged-in subject '${subject}' does not own post with ID ${postId}" )
        else
          op( db, conn )
      case None =>
        throw new ResourceNotFound(s"No post with ID ${postId} was found!")

  private def onlyIfOwner[T]( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int )( op : (PgDatabase,Connection) => T) : ZOut[T] =
    ZOut.fromTask:
      val db = appResources.database
      withConnectionTransactional( appResources.dataSource ): conn =>
        opIfOwner(db,authenticatedPoster,conn,postId)( op )

  private def onlyIfOwnerZIO[T]( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int )( op : (PgDatabase,Connection) => Task[T]) : ZOut[T] =
    ZOut.fromTask:
      val db = appResources.database
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        opIfOwner(db,authenticatedPoster,conn,postId)( op )

  private def guessMimeType( fname : String ) : Option[String] = Option(URLConnection.getFileNameMap().getContentTypeFor(fname))

  private def ensurePosterIsGrantedToDestination( db : PgDatabase, destinationIdentifier : DestinationIdentifier, subject : Subject)( conn : Connection ) : Task[Unit] =
    ensurePosterIsGrantedToDestination( db, destinationIdentifier.seismicNodeId, destinationIdentifier.name, subject )( conn ) 

  private def ensurePosterIsGrantedToDestination( db : PgDatabase, seismicNodeId : Int, destinationName : String, subject : Subject)( conn : Connection ) : Task[Unit] =
    if db.posterIsGrantedToDestination( seismicNodeId, destinationName, subject.posterId )( conn ) then
      ZIO.unit
    else
      ZIO.fail( new InsufficientPermissions( s"Poster is not granted access to destination: seismicNodeId: ${seismicNodeId}, destinationName: ${destinationName}" ) )


  /*
   *  Internal business-logic implementations for autheticated server endpoints
   */

  private def _posterInfo( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( unit : Unit ) : ZOut[PosterNoAuth] =
    ZOut.fromOptionalTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.posterById(subject.posterId)(conn).map( _.toApiPosterNoAuth )

  private def _destinations( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( unit : Unit ) : ZOut[Set[Destination]] =
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.destinationsByPosterId(subject.posterId)(conn)

  private def _newPost( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postDefinitionCreate : PostDefinitionCreate ) : ZOut[PostDefinition] =
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
            inReplyToSpecifier       = postDefinitionCreate.inReplyToSpecifier,
            authors                  = postDefinitionCreate.authors
          )( conn )
          db.postDefinitionForId( postId )( conn ).getOrElse:
            throw new ApparentBug( s"We created a new post in the database with id #${postId}, yet when we look it up there is no post?" )

  private def _destinationPosts( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( destinationIdentifier : DestinationIdentifier ) : ZOut[Set[PostDefinition]] =
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.postDefinitionsForDestinationAndOwner(destinationIdentifier.seismicNodeId, destinationIdentifier.name, subject.posterId)(conn)

  def _updatePostDefinition( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( pdu : PostDefinitionUpdate ) : ZOut[PostDefinition] =
    // println( pdu )
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )

      withConnectionTransactional(ds): conn =>
        db.postDefinitionForId( pdu.postId )( conn ) match
          case Some( currentPostDefinition ) =>
            if currentPostDefinition.owner.id == subject.posterId then
              val postId             : Int             = pdu.postId
              val title              : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.title, pdu.title )
              val postAnchor         : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.postAnchor, pdu.postAnchor )
              val sprout             : Option[Boolean] = translateNullableUpdateValueToDb(currentPostDefinition.sprout, pdu.sprout )
              val inReplyToSpecifier : Option[String]  = translateNullableUpdateValueToDb(currentPostDefinition.inReplyToSpecifier, pdu.inReplyToSpecifier )


              db.updatePostDefinitionMain(
                postId = postId,
                title = title,
                postAnchor = postAnchor,
                sprout = sprout,
                inReplyToSpecifier = inReplyToSpecifier
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

  private def _newDraft( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( npr : NewPostRevision ) : ZOut[Option[PostRevisionIdentifier]] =
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

  private def _latestDraft( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int ) : ZOut[Option[RetrievedPostRevision]] =
    def op( db : PgDatabase, conn : Connection ) : Option[RetrievedPostRevision] = db.postRevisionLatest(postId)( conn )
    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  private def _revisionHistory( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int ) : ZOut[PostRevisionHistory] =
    def op( db : PgDatabase, conn : Connection ) : PostRevisionHistory = db.postRevisionHistory(postId)( conn )
    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  private def _retrieveRevision( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( revisionTuple : (Int,Int,Int) ) : ZOut[RetrievedPostRevision] =
    val (postId,epochSeconds,nanos) = revisionTuple
    val saveTime = Instant.ofEpochSecond( epochSeconds, nanos )

    def op( db : PgDatabase, conn : Connection ) : RetrievedPostRevision =
      db.postRevisionBySaveTime(postId,saveTime)( conn ) match
        case Some(rpr) => rpr
        case None      => throw new ResourceNotFound(s"No revision with save time ${saveTime} was found!")

    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  private def _uploadPostMedia( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( uploadTuple : (Int,List[String],Option[String],ZStream[Any, Throwable, Byte]) ) : ZOut[PostMediaInfo] =
    val (postId, pathElements, contentType, stream ) = uploadTuple

    if pathElements.isEmpty then
      throw new BadMediaPath("No media path was provided for the uploaded media.")

    val fullPath = pathElements.mkString("/")

    pathElements.foreach: elem =>
      if !GoodPathElementRegex.matches(elem) then
        throw new BadMediaPath(s"Path element '$elem' contains a character not ASCII alphanumeric, dash, period, or underscore in ${fullPath}.")

    // eventually we can hit the database for user-specific limits
    val maxSize = appResources.externalConfig(ExternalConfig.Key.`protopost.media.max-length.default.mb`).toInt * 1024 * 1024

    // for now, we'll go through a temp file... it's be cool maybe to stream
    // straight into postgres, though
    def op( db : PgDatabase, conn : Connection ) : Task[PostMediaInfo] =
      val tempFile = os.temp()
      try
        def streamToFile() : Task[Long] =
          stream
            .rechunk(K32)
            .mapChunksZIO { chunk =>
              val sz = os.size(tempFile)
              //println(s"bytes uploaded: $sz")
              if sz > maxSize then
                ZIO.fail(new BadMedia(s"Exceeded maximum file size ($maxSize bytes) while uploading ''"))
              else
                ZIO.succeed(chunk)
            }
            .run(ZSink.fromFile(tempFile.toIO))
        def writeToDb() : Task[Unit] =
          ZIO.attemptBlocking:
            Using.resource( new BufferedInputStream( os.read.inputStream(tempFile) ) ): is =>
              db.upsertPostMedia(postId,fullPath,contentType,os.size(tempFile),is)( conn )
        for
          len <- streamToFile()
          _   <- writeToDb()
        yield
          PostMediaInfo(postId,fullPath,len,contentType)
      finally
        try
          os.remove(tempFile)
        catch
          case NonFatal(t) => t.printStackTrace()

    onlyIfOwnerZIO(appResources)(authenticatedPoster)(postId)(op)

  private def _postMediaByPostId( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int) : ZOut[Seq[PostMediaInfo]] =
    def op( db : PgDatabase, conn : Connection ) : Seq[PostMediaInfo] = db.postMediaInfoByPostId( postId )( conn )
    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  private def _postMedia( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( tuple : (Int,List[String]) ) : ZOut[(String,Array[Byte])] =
    val ( postId, pathElements ) = tuple

    def op( db : PgDatabase, conn : Connection ) : (String,Array[Byte]) =
      val mediaPath = pathElements.mkString("/")
      val mbOut =
        db.postMediaByPostIdMediaPath(postId,mediaPath)( conn ).map: (pmi,bytes) =>
          val ct = (pmi.`type` orElse guessMimeType(pathElements.last)).getOrElse("application/octet-stream")
          (ct, bytes)
      mbOut.getOrElse:
        throw new ResourceNotFound(s"No item found for post id $postId at media path '$mediaPath'.")

    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  private def _deletePostMedia( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( tuple : (Int,List[String]) ) : ZOut[Unit] =
    val ( postId, pathElements ) = tuple

    def op( db : PgDatabase, conn : Connection ) : Unit =
      val mediaPath = pathElements.mkString("/")
      if !db.deletePostMedia(postId,mediaPath)(conn) then
        throw new ResourceNotFound("Failed to find media for post $postId with path '${mediaPath}' in order to delete it.")

    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  private def _subscribeToRssForComments( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( rssSubscriptionRequest : RssSubscriptionRequest ) : ZOut[RssSubscriptionResponse] =
    val db = appResources.database
    val di = rssSubscriptionRequest.destinationIdentifier
    val subject = parseSubject( authenticatedPoster )

    ZOut.fromTask:
      //println( s"rssSubscriptionRequest: $rssSubscriptionRequest" )
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        for
          _ <- ensurePosterIsGrantedToDestination(db,di,subject)( conn )
          rawFeeds <- rss.findFeedsFromFeedSource( appResources.sttpClient )( rssSubscriptionRequest.feedSource )
        yield
          val subscribedFeeds = rawFeeds.map( rf => db.subscribedFeedFindCreateUpdateTitle( rf.href, rf.title, rss.DefaultFeedUpdatePeriodMins )( conn ) )
          subscribedFeeds.map( sf => db.subscribeDestinationToFeed( di.seismicNodeId, di.name, sf.id )( conn ) )
          RssSubscriptionResponse( di, subscribedFeeds.map( _.toApiSubscribableFeed ).toList )

  private def _rssSubscriptionsByDestination( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( seismicNodeId : Int, name : String ) : ZOut[Set[SubscribableFeed]] =
    val db = appResources.database
    val subject = parseSubject( authenticatedPoster )

    ZOut.fromTask:
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        for
          _ <- ensurePosterIsGrantedToDestination(db,seismicNodeId,name,subject)( conn )
        yield
          db.subscribedFeedsByDestination( seismicNodeId, name )( conn ).map( _.toApiSubscribableFeed )

  private def _deleteRssSubscription( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( seismicNodeId : Int, name : String, feedId : Int ) : ZOut[Unit] =
    val db = appResources.database
    val subject = parseSubject( authenticatedPoster )

    ZOut.fromTask:
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        for
          _ <- ensurePosterIsGrantedToDestination(db,seismicNodeId,name,subject)( conn )
        yield
          db.deleteRssSubscription(seismicNodeId,name,feedId)( conn )

  private def _mailLatestRevisionToSelf( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( postId : Int ) : ZOut[Unit] =
    val subject = parseSubject( authenticatedPoster )
    def op( db : PgDatabase, conn : Connection ) : Unit =
      val mailConfig =
        appResources.optionalMailConfig.getOrElse:
          throw new SmtpNotSupported("Server has not been configured to support sending mail.")
      given smtpContext : Smtp.Context = mailConfig.smtpContext
      val fromAddress = mailConfig.fromAddress
      val mbTitleAuthorsBody =
        for
          postDefinition <- db.postDefinitionForId( postId )( conn )
          retrievedPostRevision <- db.postRevisionLatest( postId )( conn )
        yield
          ( postDefinition.title, commaListAnd( postDefinition.authors ), retrievedPostRevision.body )
      mbTitleAuthorsBody match
        case Some( ( mbTitle, mbAuthors, body ) ) =>
          import EmailAddress.{s as es}
          val title = mbTitle.getOrElse("(untitled post)")
          val authorPart = mbAuthors.fold("")( authors => s" by ${authors}" )
          Smtp.sendSimplePlaintext( body, subject=s"""[protopost] "${title}"${authorPart}""", from=fromAddress, to=es(subject.email) ) // should we use the db email rather than trust this?
        case None =>
          throw new ResourceNotFound(s"Could not find post with ID ${postId}")

    onlyIfOwner(appResources)(authenticatedPoster)(postId)(op)

  /*
   *  Utilities for conditional cookie-extension logic
   */

  extension[T] ( base : ZOut[T] )
    def prependOptionalLowSecurityExtensionCookie( appResources : AppResources )( mbJwtExpiration : Option[JwtExpiration] ) : ZOut[(Option[CookieValueWithMeta],T)] =
      val mbCookie =
        val mbRawCookieEither =
          mbJwtExpiration.map: jwtExpiration =>
            CookieValueWithMeta.safeApply(
              jwt.Jwt.s(jwtExpiration.jwt),
              expires=Some(jwtExpiration.expiration),
              secure=appResources.inProduction,
              httpOnly=true,
              sameSite=Some(SameSite.Strict)
            )
        mbRawCookieEither.map( peel )
      base.map( t => (mbCookie, t) )

  def posterInfo( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( unit : Unit ) : ZOut[(Option[CookieValueWithMeta],PosterNoAuth)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _posterInfo( appResources )( authenticatedPoster )( unit ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def destinations( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( unit : Unit ) : ZOut[(Option[CookieValueWithMeta],Set[Destination])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _destinations( appResources )( authenticatedPoster )( unit ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def newPost( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( postDefinitionCreate : PostDefinitionCreate ) : ZOut[(Option[CookieValueWithMeta],PostDefinition)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _newPost( appResources )( authenticatedPoster )( postDefinitionCreate ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def destinationPosts( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( destinationIdentifier : DestinationIdentifier ) : ZOut[(Option[CookieValueWithMeta],Set[PostDefinition])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _destinationPosts( appResources )( authenticatedPoster )( destinationIdentifier ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def updatePostDefinition( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( pdu : PostDefinitionUpdate ) : ZOut[(Option[CookieValueWithMeta],PostDefinition)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _updatePostDefinition( appResources )( authenticatedPoster )( pdu ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def newDraft( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( npr : NewPostRevision ) : ZOut[(Option[CookieValueWithMeta],Option[PostRevisionIdentifier])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _newDraft( appResources )( authenticatedPoster )( npr ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def latestDraft( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( postId : Int ) : ZOut[(Option[CookieValueWithMeta],Option[RetrievedPostRevision])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _latestDraft( appResources )( authenticatedPoster )( postId ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def revisionHistory( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( postId : Int ) : ZOut[(Option[CookieValueWithMeta],PostRevisionHistory)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _revisionHistory( appResources )( authenticatedPoster )( postId ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def retrieveRevision( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( revisionTuple : (Int,Int,Int) ) : ZOut[(Option[CookieValueWithMeta],RetrievedPostRevision)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _retrieveRevision( appResources )( authenticatedPoster )( revisionTuple ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def uploadPostMedia( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( uploadTuple : (Int,List[String],Option[String],ZStream[Any, Throwable, Byte]) ) : ZOut[(Option[CookieValueWithMeta],PostMediaInfo)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _uploadPostMedia( appResources )( authenticatedPoster )( uploadTuple ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def postMediaByPostId( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( postId : Int) : ZOut[(Option[CookieValueWithMeta],Seq[PostMediaInfo])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _postMediaByPostId( appResources )( authenticatedPoster )( postId ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def postMedia( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( tuple : (Int,List[String]) ) : ZOut[(Option[CookieValueWithMeta],String,Array[Byte])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _postMedia( appResources )( authenticatedPoster )( tuple ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING).map:
      case ( mbCookie, ( contentType, bytes ) ) => ( mbCookie, contentType, bytes )

  def deletePostMedia( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( tuple : (Int,List[String]) ) : ZOut[Option[CookieValueWithMeta]] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _deletePostMedia( appResources )( authenticatedPoster )( tuple ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING).map:
      case ( mbCookie, unit ) => mbCookie

  def subscribeToRssForComments( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( rssSubscriptionRequest : RssSubscriptionRequest ) : ZOut[(Option[CookieValueWithMeta],RssSubscriptionResponse)] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _subscribeToRssForComments( appResources )( authenticatedPoster )( rssSubscriptionRequest ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def rssSubscriptionsByDestination( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( seismicNodeId : Int, name : String ) : ZOut[(Option[CookieValueWithMeta],Set[SubscribableFeed])] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _rssSubscriptionsByDestination( appResources )( authenticatedPoster )( seismicNodeId, name ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING)

  def deleteRssSubscription( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( seismicNodeId : Int, name : String, feedId : Int ) : ZOut[Option[CookieValueWithMeta]] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _deleteRssSubscription( appResources )( authenticatedPoster )( seismicNodeId, name, feedId ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING).map:
      case ( mbCookie, unit ) => mbCookie

  def mailLatestRevisionToSelf( appResources : AppResources )( authenticatedPosterMbJwtExpiration : AuthenticatedPosterMbJwtExpiration )( postId : Int ) : ZOut[Option[CookieValueWithMeta]] =
    val ( authenticatedPoster, mbJwtExpiration ) = authenticatedPosterMbJwtExpiration
    _mailLatestRevisionToSelf( appResources )( authenticatedPoster )( postId ).prependOptionalLowSecurityExtensionCookie( appResources )( mbJwtExpiration ).zlogErrorDefect(WARNING).map:
      case ( mbCookie, unit ) => mbCookie

end ServerLogic
