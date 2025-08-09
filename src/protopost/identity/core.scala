package protopost.identity

import com.mchange.cryptoutil.{*,given}

import com.mchange.conveniences.string.*
import java.security.interfaces.ECPublicKey
import protopost.crypto.BouncyCastleSecp256r1
import java.security.interfaces.ECPrivateKey
import protopost.{BadLocationWithIdentifierFormat,BadServiceUrl,UnknownAlgorithmOrCurve,UnsupportedProtocol}

enum Service:
  case protopost, seismic;

enum Protocol( val defaultPort : Int ):
  case http extends Protocol(80)
  case https extends Protocol(443)

object Location:
  val DefaultProtopost = Location(Protocol.http,"localhost",Protocol.http.defaultPort)
  val UrlRegex = """^(\w+)\:\/\/(localhost|(?:(?:[a-zA-Z_0-9]\.)+[a-zA-Z_0-9]+))(?:\:(\d+))?(?:\/)?$""".r
  def apply( putativeProtopostUrl : String ) : Location =
    putativeProtopostUrl match
      case UrlRegex( p, host, port ) =>
        val protocol = try Protocol.valueOf(p.toLowerCase) catch { case t : Throwable => throw new UnsupportedProtocol(p, t) }
        Location( protocol, host, if port.nullOrBlank then protocol.defaultPort else port.toInt )
      case _ => throw new BadServiceUrl( s"'${putativeProtopostUrl}' is not a valid service URL, which should look like '<protocol>://<host>/' or '<protocol>://<host>:<port>/'" )
  case class WithPath( location : Location, path : String ):
    def toUrl : String = pathJoin( location.toUrl, path )
end Location
case class Location( protocol : Protocol, host : String, port : Int ):
  def toUrl : String =
    if port == protocol.defaultPort then s"${protocol}://${host}/" else s"${protocol}://${host}:${port}/"

object PublicIdentity:
  val LocationWithIdentifierRegex = """^([^#]+)#([^:]+):\[([^>]+)>([^\]]+)\]$""".r
  def fromLocationWithIdentifier( s : String ) : PublicIdentity[?] =
    s match
      case LocationWithIdentifierRegex( location, service, algocrv, pkey ) =>
        val l = Location(location)
        val s = Service.valueOf(service)
        algocrv.toUpperCase match
          case "ES256(P-256)" =>
            val uncompressedFormatPublicKey = pkey.decodeBase64url
            ES256PublicIdentity(l, s, BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes(uncompressedFormatPublicKey))
          case _ => throw new UnknownAlgorithmOrCurve(algocrv)
      case _ => throw new BadLocationWithIdentifierFormat(s)

trait PublicIdentity[KPUB]:
  def location  : Location
  def service   : Service
  def publicKey : KPUB
  def toIdentifier : String
  def toLocationWithIdentifier : String
  
trait LocalIdentity[KPVT,KPUB]:
  def location   : Location
  def service    : Service
  def privateKey : KPVT
  def publicKey  : KPUB
  def toPublicIdentity : PublicIdentity[KPUB]

case class ES256PublicIdentity(val location : Location, val service : Service, val publicKey : ECPublicKey) extends PublicIdentity[ECPublicKey]:
  def toIdentifier: String =
    val publicKeyBase64Url = BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(publicKey).base64url
    s"${service}:[ES256(P-256)>${publicKeyBase64Url}]"
  def toLocationWithIdentifier = s"${location.toUrl}#${this.toIdentifier}"

case class ES256LocalIdentity(val location : Location, val service : Service, val privateKey : ECPrivateKey, val publicKey : ECPublicKey) extends LocalIdentity[ECPrivateKey,ECPublicKey]:
  lazy val toPublicIdentity : ES256PublicIdentity = ES256PublicIdentity(location, service, publicKey)


