package protopost.server.jwt

import com.mchange.restack.util.common.Service
import com.mchange.restack.util.server.crypto.BouncyCastleSecp256r1

import java.time.Instant
import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import java.security.interfaces.{ECPrivateKey,ECPublicKey}

enum SecurityLevel:
  case high, low

object Claims:
  enum Key:
    case securityLevel
case class Claims( keyId : String, subject : String, issuedAt : Instant, expiration : Instant, jti : String, securityLevel : SecurityLevel )

def createSignJwt( privateKey : ECPrivateKey )( keyId : String, subject : String, issuedAt : Instant, expiration : Instant, jti : String, securityLevel : SecurityLevel ) : Jwt =
  val algorithm = Algorithm.ECDSA256( null, privateKey )
  val raw =
    JWT
      .create()
      .withKeyId(keyId)
      .withSubject( subject )
      .withIssuedAt( issuedAt )
      .withExpiresAt( expiration )
      .withJWTId(jti)
      .withClaim( Claims.Key.securityLevel.toString, securityLevel.toString() )
      .sign( algorithm )
  Jwt( raw )  

def decodeVerifyJwt( publicKey : ECPublicKey )( token : Jwt ) : Claims =
  val algorithm = Algorithm.ECDSA256( publicKey, null )
  val decodedJwt = JWT.require(algorithm).build().verify( Jwt.s(token) )
  Claims (
    keyId = decodedJwt.getKeyId(),
    subject = decodedJwt.getSubject(),
    issuedAt = decodedJwt.getIssuedAtAsInstant(),
    expiration = decodedJwt.getExpiresAtAsInstant(),
    jti = decodedJwt.getId(),
    securityLevel = SecurityLevel.valueOf( decodedJwt.getClaim(Claims.Key.securityLevel.toString).asString )
  )

case class PosterAuthInfo(highSecurityToken : Option[String], lowSecurityToken : Option[String])
case class AuthenticatedPoster(claims : Claims, level : SecurityLevel)

// typewrappers

object Jwt:
  private[protopost] inline def apply( s : String ) : Jwt = s
  private[protopost] inline def s(jwt : Jwt) : String = jwt
opaque type Jwt = String


