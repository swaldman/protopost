package protopost.api

import zio.*

import sttp.model.StatusCode
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.{AppResources,ExternalConfig,MissingConfig,PosterId,jwt}
import protopost.jwt.{Jwk,Jwks}

import protopost.db.PgDatabase

import com.mchange.rehash.*

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.mchange.sc.zsqlutil.*
import protopost.BadCredentials

object TapirEndpoint:

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

  val Login = Base.post.in("login").in(jsonBody[EmailPassword]).out(jsonBody[Jwts])

  private val JtiEntropyBytes = 16

  private def newJti( appResources : AppResources ) : String =
    import com.mchange.cryptoutil.{*,given}
    val arr = Array.ofDim[Byte](JtiEntropyBytes)
    appResources.entropy.nextBytes(arr)
    arr.base64url

  def jwks( appResources : AppResources )(u : Unit) : ZOut[Jwks] =
    ZOut.fromTask:
      ZIO.attempt:
        val identity = appResources.localIdentity
        val jwk = Jwk( identity.publicKey, identity.service )
        Jwks( List( jwk ) )

  def login( appResources : AppResources )( emailPassword : EmailPassword ) : ZOut[Jwts] =
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
            appResources.authManager.verifyRehash( pwa.id, password, fetchHash, storeHash ) match
              case OK => ZIO.unit
              case WrongPassword => ZIO.fail( new BadCredentials( "The password given fails to validate." ) )
              case UserNotFound => ZIO.fail( new BadCredentials( s"No poster found with id '${pwa.id}', despite identification of this poster by email ${email}?!?" ) )
          case None =>
            ZIO.fail( new BadCredentials( s"No poster found with e-mail '${emailPassword.email}'." ) )
    val issueTokens = ZIO.attempt:
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
      Jwts( highSecurityJwt, lowSecurityJwt )
    ZOut.fromTask:
      checkCredentials *> issueTokens

  def serverEndpoints( appResources : AppResources ) : List[ZServerEndpoint[Any,Any]] =
    List (
      //RootJwks.zServerLogic( jwks( appResources ) ),
      WellKnownJwks.zServerLogic( jwks( appResources ) ),
      Login.zServerLogic( login( appResources ) )
    )

end TapirEndpoint
