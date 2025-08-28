package protopost.api

import zio.*

import sttp.model.StatusCode
import sttp.model.headers.{CookieValueWithMeta,Cookie}
import Cookie.SameSite
import sttp.tapir.files.*
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.{AppResources,EmailAddress,ExternalConfig,MissingConfig,Password,PosterId}
import protopost.LoggingApi.*

import protopost.db.PgDatabase

import com.mchange.rehash.*

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.mchange.sc.zsqlutil.*
import protopost.{BadCookieSettings,BadCredentials,NotLoggedIn,jwt}
import sttp.model.headers.CookieValueWithMeta

object TapirEndpoint extends SelfLogging:

  val service = protopost.common.Service.protopost // forseeing abstracting some of this to a more abstract restack library

  val errorHandler =
    def errorBodyOutNotFound() = stringBody.map(_ => None)( _ => "Resource not found." )
    def errorBodyOut[T <: Throwable]( throwableClass : Class[T] ) =
      stringBody.map(fst => ReconstructableThrowable(Some(throwableClass),fst))(_.fullStackTrace)
    def errorBodyOutLostThrowableClass() =
      stringBody.map(fst => ReconstructableThrowable(None,fst))(_.fullStackTrace)
    oneOf[ReconstructableThrowable | None.type](
      oneOfVariantValueMatcher(statusCode(StatusCode.NotFound).and(errorBodyOutNotFound())){ case _ : None.type => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.Forbidden).and(errorBodyOut(classOf[BadCredentials]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[BadCredentials]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.Unauthorized).and(errorBodyOut(classOf[NotLoggedIn]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[NotLoggedIn]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.InternalServerError).and(errorBodyOutLostThrowableClass())){ case rt : ReconstructableThrowable if rt.throwableClass == None => true },
  )

  val NakedBase = endpoint.errorOut(errorHandler)
  val Base = NakedBase.in("protopost")
  val PosterAuthenticated = Base.securityIn(
    cookie[Option[String]]("token_security_high")
      .and(cookie[Option[String]]("token_security_low"))
      .mapTo[jwt.PosterAuthInfo]
  )

  val Envelope = Base.in("envelope")

  //val RootJwks = Base.in("jwks.json").out(jsonBody[Jwks])
  val WellKnownJwks = NakedBase.in(".well-known").in("jwks.json").out(jsonBody[jwt.Jwks])

  val Login =
    Base.post
      .in("login")
      .in(jsonBody[EmailPassword])
      .out(setCookie("token_security_high"))
      .out(setCookie("token_security_low"))
      .out(jsonBody[LoginStatus])

  val Logout =
    Base.post
      .in("logout")
      .out(setCookie("token_security_high"))
      .out(setCookie("token_security_low"))
      .out(jsonBody[LoginStatus])

  val LoginStatus =
    Base.get
      .in("login-status")
      .in(cookie[Option[String]]("token_security_high"))
      .in(cookie[Option[String]]("token_security_low"))
      .out(jsonBody[LoginStatus])

  val Client = Base.get.in("client").in("top.html").out(htmlBodyUtf8)
  val RootAsClient = NakedBase.in("").get.out(htmlBodyUtf8)

  val PosterInfo = PosterAuthenticated.get.in("poster-info").out(jsonBody[PosterNoAuth])

  val Destinations = PosterAuthenticated.get.in("destinations").out(jsonBody[Set[DestinationNickname]])

  val ScalaJsServerEndpoint = staticResourcesGetServerEndpoint[[x] =>> zio.RIO[Any, x]]("protopost"/"client"/"scalajs")(this.getClass().getClassLoader(), "scalajs")

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

  def destinations( appResources : AppResources )( authenticatedPoster : jwt.AuthenticatedPoster )( unit : Unit ) : ZOut[Set[DestinationNickname]] =
    ZOut.fromTask:
      val db = appResources.database
      val ds = appResources.dataSource
      val subject = parseSubject( authenticatedPoster )
      withConnectionTransactional(ds): conn =>
        db.destinationNicknamesByPosterId(subject.posterId)(conn)

  def jwks( appResources : AppResources )(u : Unit) : ZOut[jwt.Jwks] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity
        val jwk = jwt.Jwk( identity.publicKey, identity.service )
        jwt.Jwks( List( jwk ) )

  def client( appResources : AppResources )(unit : Unit) : ZOut[String] =
    ZOut.fromTask( ZIO.attempt(protopost.client.client_top_html( appResources.localIdentity.location.toUrl ).text) )

  extension ( pwd : protopost.Password )
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
        (peel(highSecurityCookieValue), peel(lowSecurityCookieValue), protopost.api.LoginStatus.empty)

  private def loginStatusFromExpirations( highSecurityExpiration : Instant, lowSecurityExpiration : Instant ) : protopost.api.LoginStatus =
    val nowEpochSecond = java.lang.System.currentTimeMillis() / 1000
    val highSecuritySecondsRemaining = math.max(toEpochSecond(highSecurityExpiration) - nowEpochSecond, 0L)
    val lowSecuritySecondsRemaining = math.max(toEpochSecond(lowSecurityExpiration) - nowEpochSecond, 0L)
    protopost.api.LoginStatus( highSecuritySecondsRemaining, lowSecuritySecondsRemaining )

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

  def serverEndpoints( appResources : AppResources ) : List[ZServerEndpoint[Any,Any]] =
    val rootAsClient =
      appResources.externalConfig
        .get( ExternalConfig.Key.`protopost.api.root-as-client` )
        .map( java.lang.Boolean.parseBoolean )
        .flatMap( use => if use then Some( RootAsClient.zServerLogic( client( appResources ) ) : ZServerEndpoint[Any,Any] ) else None )
    List[sttp.tapir.ztapir.ZServerEndpoint[Any, Any]] (
      //RootJwks.zServerLogic( jwks( appResources ) ),
      WellKnownJwks.zServerLogic( jwks( appResources ) ),
      Login.zServerLogic( login( appResources ) ),
      LoginStatus.zServerLogic( loginStatus( appResources ) ),
      Logout.zServerLogic( logout( appResources ) ),
      Client.zServerLogic( client( appResources ) ),
      PosterInfo.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( posterInfo(appResources) ),
      Destinations.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( destinations(appResources) ),
      ScalaJsServerEndpoint
    ) ++ rootAsClient.toList

end TapirEndpoint
