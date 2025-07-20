package protopost.jwt

import java.time.Instant
import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import java.security.interfaces.{ECPrivateKey,ECPublicKey}

import protopost.Server
import protopost.crypto.BouncyCastleSecp256r1

enum SecurityLevel:
  case high, low

object Claims:
  enum Key:
    case securityLevel
case class Claims( keyId : String, subject : String, issuedAt : Instant, expiration : Instant, securityLevel : SecurityLevel )

object Jwk:
  val DefaultKty = "EC"
  val DefaultCrv = "P-256"
  val DefaultAlg = "ES256"
  val DefaultUse = "sig"
  def apply( publicKey : ECPublicKey, location : Server.Location ) : Jwk =
    Jwk(
      x = BouncyCastleSecp256r1.fieldValueToBase64Url( publicKey.getW().getAffineX() ),
      y = BouncyCastleSecp256r1.fieldValueToBase64Url( publicKey.getW().getAffineY() ),
      kid = location.toUrl,
      kty = DefaultKty,
      crv = DefaultCrv,
      alg = DefaultAlg,
      use = DefaultUse
    )
case class Jwk( x : String, y : String, kid : String, kty : String, crv : String, alg : String, use : String )
case class Jwks( keys : List[Jwk] )

def createSignJwt( privateKey : ECPrivateKey )( keyId : String, subject : String, issuedAt : Instant, expiration : Instant, securityLevel : SecurityLevel ) : Jwt =
  val algorithm = Algorithm.ECDSA256( null, privateKey )
  val raw =
    JWT
      .create()
      .withKeyId(keyId)
      .withSubject( subject )
      .withIssuedAt( issuedAt )
      .withExpiresAt( expiration )
      .withClaim( Claims.Key.securityLevel.toString, securityLevel.toString() )
      .sign( algorithm )
  Jwt( raw )  

def decodeVerifyJwt( publicKey : ECPublicKey )( token : Jwt ) : Claims =
  val algorithm = Algorithm.ECDSA256( publicKey, null )
  val decodedJwt = JWT.require(algorithm).build().verify( token.str )
  Claims (
    keyId = decodedJwt.getKeyId(),
    subject = decodedJwt.getSubject(),
    issuedAt = decodedJwt.getIssuedAtAsInstant(),
    expiration = decodedJwt.getExpiresAtAsInstant(),
    securityLevel = SecurityLevel.valueOf( decodedJwt.getClaim(Claims.Key.securityLevel.toString).asString )
  )

