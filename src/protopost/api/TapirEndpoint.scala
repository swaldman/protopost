package protopost.api

import zio.*

import sttp.model.StatusCode
import sttp.model.headers.{CookieValueWithMeta,Cookie}
import Cookie.SameSite
import sttp.tapir.files.*
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.{AppResources,ExternalConfig,MissingConfig,Password,PosterId,jwt}
import protopost.jwt.{Jwk,Jwks,decodeVerifyJwt}
import protopost.LoggingApi.*

import protopost.db.PgDatabase

import com.mchange.rehash.*

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.mchange.sc.zsqlutil.*
import protopost.{BadCookieSettings,BadCredentials}
import sttp.model.headers.CookieValueWithMeta


object TapirEndpoint extends SelfLogging:

  val service = protopost.identity.Service.protopost // forseeing abstracting some of this to a more abstract restack library

  val errorHandler =
    def errorBodyOut[T <: Throwable]( throwableClass : Class[T] ) =
      stringBody.map(fst => ReconstructableThrowable(Some(throwableClass),fst))(_.fullStackTrace)
    def errorBodyOutNone() =
      stringBody.map(fst => ReconstructableThrowable(None,fst))(_.fullStackTrace)
    oneOf[ReconstructableThrowable](
      oneOfVariantValueMatcher(statusCode(StatusCode.Forbidden).and(errorBodyOut(classOf[BadCredentials]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[BadCredentials]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.InternalServerError).and(errorBodyOutNone())){ case rt : ReconstructableThrowable if rt.throwableClass == None => true },
  )

  val NakedBase = endpoint.errorOut(errorHandler)
  val Base = NakedBase.in("protopost")
  val Envelope = Base.in("envelope")

  //val RootJwks = Base.in("jwks.json").out(jsonBody[Jwks])
  val WellKnownJwks = NakedBase.in(".well-known").in("jwks.json").out(jsonBody[Jwks])

  val Login =
    Base.post
      .in("login")
      .in(jsonBody[EmailPassword])
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

  val ScalaJsServerEndpoint = staticResourcesGetServerEndpoint[[x] =>> zio.RIO[Any, x]]("protopost"/"client"/"scalajs")(this.getClass().getClassLoader(), "scalajs")

  private val JtiEntropyBytes = 16

  private def newJti( appResources : AppResources ) : String =
    import com.mchange.cryptoutil.{*,given}
    val arr = Array.ofDim[Byte](JtiEntropyBytes)
    appResources.entropy.nextBytes(arr)
    arr.base64url

  private def toEpochSecond( instant : Instant ) = instant.toEpochMilli / 1000

  def jwks( appResources : AppResources )(u : Unit) : ZOut[Jwks] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity
        val jwk = Jwk( identity.publicKey, identity.service )
        Jwks( List( jwk ) )

  def client( appResources : AppResources )(unit : Unit) : ZOut[String] =
    ZOut.fromTask( ZIO.attempt(protopost.client.client_top_html( appResources.localIdentity.location.toUrl ).text) )

  extension ( pwd : protopost.Password )
    def toRehash : com.mchange.rehash.Password = com.mchange.rehash.Password(Password.s(pwd))

  def login( appResources : AppResources )( emailPassword : EmailPassword ) : ZOut[(CookieValueWithMeta, CookieValueWithMeta, LoginStatus)] =
    import com.mchange.rehash.str, protopost.str

    val email = emailPassword.email
    val password = emailPassword.password
    val database = appResources.database
    val identity = appResources.localIdentity

    val checkCredentials =
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        import VerificationResult.*
        database.posterWithAuthForEmail( conn, email ) match
          case Some( pwa ) =>
            val fetchHash : PosterId => Option[BCryptHash] = posterId => database.fetchHashForPoster( conn, posterId )
            val storeHash : ( PosterId, BCryptHash ) => Unit = ( posterId : PosterId, hash : BCryptHash ) => database.updateHashForPoster( conn, posterId, hash )
            appResources.authManager.verifyRehash( pwa.id, password.toRehash, fetchHash, storeHash ) match
              case OK => ZIO.unit
              case WrongPassword => ZIO.fail( new BadCredentials( "The password given fails to validate." ) )
              case UserNotFound => ZIO.fail( new BadCredentials( s"No poster found with id '${pwa.id}', despite identification of this poster by email ${email}?!?" ) )
          case None =>
            ZIO.fail( new BadCredentials( s"No poster found with e-mail '${emailPassword.email}'." ) )
    val issueTokens : Task[(CookieValueWithMeta,CookieValueWithMeta,LoginStatus)] =
      ZIO.attempt:
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
          subject = email.str,
          issuedAt = issuedAt,
          expiration = highSecurityExpiration,
          jti = highSecurityJti,
          securityLevel = jwt.SecurityLevel.high
        )
        val lowSecurityJwt = jwt.createSignJwt( appResources.localIdentity.privateKey )(
          keyId = service.toString,
          subject = email.str,
          issuedAt = issuedAt,
          expiration = lowSecurityExpiration,
          jti = lowSecurityJti,
          securityLevel = jwt.SecurityLevel.low
        )
        import protopost.jwt.str
        val highSecurityCookieValue = CookieValueWithMeta.safeApply(highSecurityJwt.str, expires=Some(highSecurityExpiration), secure=appResources.inProduction, httpOnly=true, sameSite=Some(SameSite.Strict))
        val lowSecurityCookieValue  = CookieValueWithMeta.safeApply(lowSecurityJwt.str,  expires=Some(lowSecurityExpiration),  secure=appResources.inProduction, httpOnly=true, sameSite=Some(SameSite.Strict))

        def peel( either : Either[String,CookieValueWithMeta] ) =
          either match
            case Left( str )   => throw new BadCookieSettings( str )
            case Right( cvwm ) => cvwm

        ( peel(highSecurityCookieValue), peel(lowSecurityCookieValue), loginStatusFromExpirations(highSecurityExpiration, lowSecurityExpiration) )

    ZOut.fromTask:
      checkCredentials *> issueTokens

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
                val jwt = protopost.jwt.Jwt(tokenStr)
                val verified = decodeVerifyJwt(identity.publicKey)(jwt)
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
    List (
      //RootJwks.zServerLogic( jwks( appResources ) ),
      WellKnownJwks.zServerLogic( jwks( appResources ) ),
      Login.zServerLogic( login( appResources ) ),
      LoginStatus.zServerLogic( loginStatus( appResources ) ),
      Client.zServerLogic( client( appResources ) ),
      ScalaJsServerEndpoint
    )

end TapirEndpoint
