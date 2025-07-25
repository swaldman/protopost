package protopost.api

import zio.*

import protopost.{EmailAddress,Server,SignatureDoesNotVerify,str}
import protopost.crypto.{*,given}
import protopost.jwt.{Jwk,Jwks,Jwt,str}

import com.mchange.conveniences.throwable.*
import com.mchange.cryptoutil.*

import java.security.interfaces.ECPublicKey

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.Schema
import java.security.interfaces.ECPrivateKey
import java.util.Base64

import scala.collection.immutable

import com.mchange.rehash.{Password,str}

// stealing some utilities from https://github.com/swaldman/hotsauce-devilla

case class ReconstructableThrowable( throwableClass : Option[Class[?]], fullStackTrace : String )

object ZOut:
  def fromTask[U]( task : Task[U] ) : ZOut[U] = task.mapError( t => ReconstructableThrowable( Some(t.getClass()), t.fullStackTrace ) )
type ZOut[T] = ZIO[Any,ReconstructableThrowable,T]

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

case class Jwts( highSecurity : Jwt, lowSecurity : Jwt )

case class EmailPassword( email : EmailAddress, password : Password )

// json codecs -- jsoniter-scala
given JsonValueCodec[EmailAddress] = new JsonValueCodec[EmailAddress]:
  def decodeValue(in : JsonReader, default : EmailAddress) : EmailAddress = EmailAddress(in.readString(null))
  def encodeValue(x : EmailAddress, out: JsonWriter): Unit = out.writeVal(x.str)
  def nullValue : EmailAddress = null.asInstanceOf[EmailAddress]
given JsonValueCodec[Password] = new JsonValueCodec[Password]:
  def decodeValue(in : JsonReader, default : Password) : Password = Password(in.readString(null))
  def encodeValue(x : Password, out: JsonWriter): Unit = out.writeVal(x.str)
  def nullValue : Password = null.asInstanceOf[Password]
given JsonValueCodec[Jwt] = new JsonValueCodec[Jwt]:
  def decodeValue(in: JsonReader, default: Jwt): Jwt = Jwt(in.readString(null))
  def encodeValue(x: Jwt, out: JsonWriter): Unit     = out.writeVal(x.str)
  def nullValue: Jwt                                 = null.asInstanceOf[Jwt]

given JsonValueCodec[Jwk]           = JsonCodecMaker.make
given JsonValueCodec[Jwks]          = JsonCodecMaker.make
given JsonValueCodec[Jwts]          = JsonCodecMaker.make
given JsonValueCodec[Envelope]      = JsonCodecMaker.make
given JsonValueCodec[EmailPassword] = JsonCodecMaker.make

// json codecs -- tapir
given Schema[EmailAddress]  = Schema.string.map((s : String) => Some(EmailAddress(s)))(addr => addr.str)
given Schema[Password]      = Schema.string.map((s : String) => Some(Password(s)))(pw => pw.str)
given Schema[Jwt]           = Schema.string.map((s : String) => Some(Jwt(s)))(jwt => jwt.str)
given Schema[Jwk]           = Schema.derived
given Schema[Jwks]          = Schema.derived
given Schema[Jwts]          = Schema.derived
given Schema[Envelope]      = Schema.derived
given Schema[EmailPassword] = Schema.derived

