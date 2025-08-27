package protopost.identity

import com.mchange.cryptoutil.{*,given}

import protopost.{BadIdentifierFormat,UnknownAlgorithmOrCurve}
import protopost.crypto.BouncyCastleSecp256r1

import java.security.PublicKey
import java.security.interfaces.{ECPrivateKey,ECPublicKey}

import scala.collection.immutable

object PublicIdentity:
  trait WithAlgcrv:
    def algcrv : String
  object ES256 extends WithAlgcrv:
    val algcrv : String = "ES256(P-256)"
  case class ES256(val location : Location.Simple, val service : Service, val publicKey : ECPublicKey) extends PublicIdentity[ECPublicKey]:
    val algcrv : String = ES256.algcrv
    lazy val publicKeyBytes : immutable.ArraySeq[Byte] = immutable.ArraySeq.ofByte(BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes( publicKey ))
    def toIdentifier: String =
      val publicKeyHex = BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(publicKey).hex0x
      s"${service}[${algcrv}]${publicKeyHex}"
    def toIdentifierWithLocation = s"${this.toIdentifier}:${location.toUrl}"

  val IdentifierWithLocationRegex = """^([^\[]+)\[([^\]]+)\]([^\:]+)\:(.+)$""".r
  def fromIdentifierWithLocationAny( s : String ) : (PublicIdentity[?], Option[String]) =
    s match
      case IdentifierWithLocationRegex( service, algcrv, pkey, location ) =>
        val la = Location(location)
        val s = Service.valueOf(service)
        val proto = Proto.PublicIdentity(s, algcrv, pkey, la.protocol, la.host, la.port)
        val pi = proto.complete
        val path =
          la match
            case lwp : Location.WithPath => Some(lwp.path)
            case ls  : Location.Simple     => None
        (pi, path)  
      case _ => throw new BadIdentifierFormat(s)
  def fromIdentifierWithLocationSimple( s : String ) : PublicIdentity[?] =
    fromIdentifierWithLocationAny( s ) match
      case ( pi, None ) => pi
      case _            => throw new BadIdentifierFormat("Expected a simple identifier, but found an identifier with a path component: " + s)
  def apply( service : Service, algcrv : String, pubKeyBytes : Array[Byte], location : Location.Simple ) : PublicIdentity[?] =
    algcrv.toUpperCase match
      case "ES256(P-256)" => ES256(location, service, BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes(pubKeyBytes))
      case _ => throw new UnknownAlgorithmOrCurve(algcrv)
trait PublicIdentity[KPUB <: PublicKey] extends PublicIdentity.WithAlgcrv:
  def location  : Location.Simple
  def service   : Service
  def algcrv    : String
  def publicKey : KPUB
  def publicKeyBytes : immutable.ArraySeq[Byte]
  def toIdentifier : String
  def toIdentifierWithLocation : String
