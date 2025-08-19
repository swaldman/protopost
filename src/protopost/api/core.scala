package protopost.api

import zio.*

import protopost.{EmailAddress,Password,PosterId,SignatureDoesNotVerify}
import protopost.crypto.{*,given}

import protopost.jwt.{Jwk,Jwks,Jwt}

import com.mchange.conveniences.throwable.*
import com.mchange.cryptoutil.{*,given}

import java.security.interfaces.ECPublicKey

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.Schema
import java.security.interfaces.ECPrivateKey
import java.util.Base64

import scala.collection.immutable

case class ReconstructableThrowable( throwableClass : Option[Class[?]], fullStackTrace : String )

object ZOut:
  def fromTask[U]( task : Task[U] ) : ZOut[U] = task.mapError( t => ReconstructableThrowable( Some(t.getClass()), t.fullStackTrace ) )
  def fromOptionalTask[U]( task : Task[Option[U]] ) : ZOut[U] =
    fromTask(task).flatMap:
      case Some(u) => ZIO.succeed(u)
      case None    => ZIO.fail(None)
type ZOut[T] = ZIO[Any,ReconstructableThrowable | None.type,T]

object Envelope:
  def apply( messageBytes : Array[Byte], privateKey : ECPrivateKey ) : Envelope =
    val signature = BouncyCastleSecp256r1.sign( messageBytes, privateKey )
    val signer = BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(BouncyCastleSecp256r1.publicKeyFromPrivate(privateKey))
    Envelope( messageBytes.base64url, signature.unsafeInternalArray.base64url, signer.base64url )
  def wrap[T : JsonValueCodec]( messageThang : T, privateKey : ECPrivateKey ) : Envelope = this.apply( writeToArray(messageThang), privateKey )
  def verifyUnwrap[T : JsonValueCodec]( envelope : Envelope ) : T =
    val messageBytes = envelope.message.toArray  // XXX: should I use unsafeArray?
    if BouncyCastleSecp256r1.verify( messageBytes, envelope.signature, envelope.signer ) then
      readFromArray[T]( messageBytes )
    else
      throw new SignatureDoesNotVerify( s"The signature of " + envelope + " does not verify." )

case class Envelope( messageBase64url : String, signatureBase64url : String, signerBase64url : String ):
  lazy val message : immutable.ArraySeq[Byte] = immutable.ArraySeq.ofByte( Base64.getUrlDecoder().decode( messageBase64url ) )
  lazy val signature : SignatureSHA256withECDSA = SignatureSHA256withECDSA( Base64.getUrlDecoder().decode( signatureBase64url ) )
  lazy val signer : ECPublicKey = BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes( Base64.getUrlDecoder().decode( signerBase64url ) )
  lazy val hash : Hash.SHA3_256 = Hash.SHA3_256.hash( message.toArray ++ signature.unsafeInternalArray ++ BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(signer) )
  override def toString = s"Envelope[${hash.hex0x}]"

//case class Jwts( highSecurity : Jwt, lowSecurity : Jwt )

// json codecs -- jsoniter-scala
given JsonValueCodec[Jwt] = new JsonValueCodec[Jwt]:
  def decodeValue(in: JsonReader, default: Jwt): Jwt = Jwt(in.readString(null))
  def encodeValue(x: Jwt, out: JsonWriter): Unit     = out.writeVal(Jwt.s(x))
  def nullValue: Jwt                                 = null.asInstanceOf[Jwt]

given JsonValueCodec[Jwk]           = JsonCodecMaker.make
given JsonValueCodec[Jwks]          = JsonCodecMaker.make
//given JsonValueCodec[Jwts]          = JsonCodecMaker.make
given JsonValueCodec[Envelope]      = JsonCodecMaker.make

// json codecs -- tapir
given Schema[EmailAddress]  = Schema.string.map((s : String) => Some(EmailAddress(s)))(addr => EmailAddress.s(addr))
given Schema[Password]      = Schema.string.map((s : String) => Some(Password(s)))(pw => Password.s(pw))
given Schema[Jwt]           = Schema.string.map((s : String) => Some(Jwt(s)))(jwt => Jwt.s(jwt))
given Schema[PosterId]      = Schema.schemaForInt.map( (i : Int) => Some(PosterId(i)) )(pid => PosterId.i(pid))
given Schema[Jwk]           = Schema.derived
given Schema[Jwks]          = Schema.derived
//given Schema[Jwts]          = Schema.derived
given Schema[LoginStatus]   = Schema.derived
given Schema[Envelope]      = Schema.derived
given Schema[EmailPassword] = Schema.derived
given Schema[PosterNoAuth] = Schema.derived

