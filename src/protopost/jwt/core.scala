package protopost.jwt

import java.time.Instant
import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import java.security.interfaces.{ECPrivateKey,ECPublicKey}

enum SecurityLevel:
  case high, low

object Claims:
  enum Key:
    case securityLevel
case class Claims( subject : String, issuedAt : Instant, expiration : Instant, securityLevel : SecurityLevel )

def createSignJwt( privateKey : ECPrivateKey )( subject : String, issuedAt : Instant, expiration : Instant, securityLevel : SecurityLevel ) : String =
  val algorithm = Algorithm.ECDSA256( null, privateKey )
  JWT
    .create()
    .withSubject( subject )
    .withIssuedAt( issuedAt )
    .withExpiresAt( expiration )
    .withClaim( Claims.Key.securityLevel.toString, securityLevel.toString() )
    .sign( algorithm )

def decodeVerifyJwt( publicKey : ECPublicKey )( token : String ) : Claims =
  val algorithm = Algorithm.ECDSA256( publicKey, null )
  val decodedJwt = JWT.require(algorithm).build().verify( token )
  Claims (
    subject = decodedJwt.getSubject(),
    issuedAt = decodedJwt.getIssuedAtAsInstant(),
    expiration = decodedJwt.getExpiresAtAsInstant(),
    securityLevel = SecurityLevel.valueOf( decodedJwt.getClaim(Claims.Key.securityLevel.toString).asString )
  )

