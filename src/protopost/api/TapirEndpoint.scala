package protopost.api

import zio.*

import sttp.model.StatusCode
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.{AppResources,ExternalConfig,MissingConfig,PosterId,Server,jwt}

import protopost.db.PgDatabase

import com.mchange.reauth.*

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.mchange.sc.zsqlutil.*
import protopost.BadCredentials

object TapirEndpoint:

  // stealing from https://github.com/swaldman/hotsauce-devilla
  val either404or500 = oneOf[Option[String]](
    oneOfVariantValueMatcher(statusCode(StatusCode.NotFound).and(stringBody.map(s => None)(_ => "Not Found."))){ case None => true },
    oneOfVariantValueMatcher(statusCode(StatusCode.InternalServerError).and(stringBody.map(s => Some(s))(_.get))){ case Some(_) => true }
  )

  val Base = endpoint.errorOut(either404or500)
  val EnvelopeBase = endpoint.in("envelope")

  val RootJwks = Base.in("jwks.json").out(jsonBody[Jwks])
  val WellKnownJwks = Base.in(".well-known").in("jwks.json").out(jsonBody[Jwks])

  val Login = Base.post.in("login").in(jsonBody[EmailPassword]).out(jsonBody[Jwts])

  def jwks( appResources : AppResources )(u : Unit) : ZOut[Jwks] =
    val task =
      ZIO.attempt:
        val identity = Server.Identity( appResources.externalConfig )
        val jwk = Jwk( identity.publicKey, identity.location )
        Jwks( List( jwk ) )
    mapPlainError( task )

  def login( appResources : AppResources )( emailPassword : EmailPassword ) : ZOut[Jwts] =
    import com.mchange.reauth.str, protopost.str
    def throwNoPosterEmail = throw new BadCredentials( s"No poster found with e-mail '${emailPassword.email}'." )

    val email = emailPassword.email
    val password = emailPassword.password
    val database = appResources.database

    val checkCredentials =
      withConnectionTransactionalZIO( appResources.dataSource ): conn =>
        import VerificationResult.*
        val pwa = database.posterWithAuthForEmail( conn, email ).getOrElse( throwNoPosterEmail )
        val fetchHash : PosterId => Option[BCryptHash] = posterId => database.fetchHashForPoster( conn, posterId )
        val storeHash : ( PosterId, BCryptHash ) => Unit = ( posterId : PosterId, hash : BCryptHash ) => database.updateHashForPoster( conn, posterId, hash )
        appResources.authManager.verifyRehash( pwa.id, password, fetchHash, storeHash ) match
          case OK => ZIO.unit
          case WrongPassword => ZIO.fail( new BadCredentials( "The password given fails to validate." ) )
          case UserNotFound => ZIO.fail( new BadCredentials( s"No poster found with id '${pwa.id}', despite identification of this poster by email ${email}?!?" ) )
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
      val highSecurityJwt = jwt.createSignJwt( appResources.keyPair(0) )(
        subject = email.str,
        issuedAt = issuedAt,
        expiration = highSecurityExpiration,
        securityLevel = jwt.SecurityLevel.high
      )
      val lowSecurityJwt = jwt.createSignJwt( appResources.keyPair(0) )(
        subject = email.str,
        issuedAt = issuedAt,
        expiration = lowSecurityExpiration,
        securityLevel = jwt.SecurityLevel.low
      )
      Jwts( highSecurityJwt, lowSecurityJwt )
    val task = checkCredentials *> issueTokens
    mapPlainError( task )

  def serverEndpoints( appResources : AppResources ) : List[ZServerEndpoint[Any,Any]] =
    List (
      RootJwks.zServerLogic( jwks( appResources ) ),
      WellKnownJwks.zServerLogic( jwks( appResources ) ),
      Login.zServerLogic( login( appResources ) )
    )

end TapirEndpoint
