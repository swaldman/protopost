package protopost.api

import zio.*

import protopost.{Server,SignatureDoesNotVerify}
import protopost.crypto.{*,given}
//import protopost.crypto.unsafeInternalArray

import com.mchange.conveniences.throwable.*
import com.mchange.cryptoutil.*

import java.security.interfaces.ECPublicKey

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.Schema
import java.security.interfaces.ECPrivateKey
import java.util.Base64

import scala.collection.immutable

// stealing some utilities from https://github.com/swaldman/hotsauce-devilla

type ZOut[T] = ZIO[Any,Option[String],T]

def mapPlainError[U]( task : Task[U] ) : ZOut[U] = task.mapError( t => Some( t.fullStackTrace ) )

def mapMaybeError[U]( task : Task[Option[U]] ) : ZOut[U] =
  mapPlainError( task ).flatMap:
    case Some( u ) => ZIO.succeed( u )
    case None      => ZIO.fail[Option[String]]( None )

object Jwk:
  val DefaultKty = "EC"
  val DefaultCrv = "P-256"
  def apply( publicKey : ECPublicKey, location : Server.Location ) : Jwk =
    Jwk(
      x = BouncyCastleSecp256r1.fieldValueToHex( publicKey.getW().getAffineX() ),
      y = BouncyCastleSecp256r1.fieldValueToHex( publicKey.getW().getAffineY() ),
      kid = location.toUrl,
      kty = DefaultKty,
      crv = DefaultCrv
    )
case class Jwk( x : String, y : String, kid : String, kty : String, crv : String )
case class Jwks( keys : List[Jwk] )

object Envelope:
  def apply( messageBytes : Array[Byte], privateKey : ECPrivateKey ) : Envelope =
    val signature = BouncyCastleSecp256r1.sign( messageBytes, privateKey )
    val messageBase64 = Base64.getEncoder().encodeToString(messageBytes)
    val signatureBase64 = Base64.getEncoder().encodeToString(signature.unsafeInternalArray)
    val signerBase64 = Base64.getEncoder().encodeToString(BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(BouncyCastleSecp256r1.publicKeyFromPrivate(privateKey)))
    Envelope( messageBase64, signatureBase64, signerBase64 )
  def wrap[T : JsonValueCodec]( messageThang : T, privateKey : ECPrivateKey ) : Envelope = this.apply( writeToArray(messageThang), privateKey )
  def verifyUnwrap[T : JsonValueCodec]( envelope : Envelope ) : T =
    val messageBytes = envelope.message.toArray  // XXX: should I use unsfaeArray?
    if BouncyCastleSecp256r1.verify( messageBytes, envelope.signature, envelope.signer ) then
      readFromArray[T]( messageBytes )
    else
      throw new SignatureDoesNotVerify( s"The signature of " + envelope + " does not verify." )

  case class Envelope( messageBase64 : String, signatureBase64 : String, signerBase64 : String ):
    lazy val message : immutable.ArraySeq[Byte] = immutable.ArraySeq.ofByte( Base64.getDecoder().decode( messageBase64 ) )
    lazy val signature : SignatureSHA256withECDSA = SignatureSHA256withECDSA( Base64.getDecoder().decode( signatureBase64 ) )
    lazy val signer : ECPublicKey = BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes( Base64.getDecoder().decode( signerBase64 ) )
    lazy val hash : Hash.SHA3_256 = Hash.SHA3_256.hash( message.toArray ++ signature.unsafeInternalArray ++ BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(signer) )
    override def toString = s"Envelope[${hash.hex0x}]"

// json codecs -- jsoniter-scala
given JsonValueCodec[Jwk]  = JsonCodecMaker.make
given JsonValueCodec[Jwks] = JsonCodecMaker.make

// json codecs -- tapir
given Schema[Jwk]  = Schema.derived
given Schema[Jwks] = Schema.derived

