package protopost.identity

import protopost.{BadRestackUrl,UnknownRestackProtocol}

import com.mchange.cryptoutil.{*,given}

import com.mchange.conveniences.string.*
import java.security.interfaces.ECPublicKey
import protopost.crypto.BouncyCastleSecp256r1
import java.security.interfaces.ECPrivateKey

enum Service:
  case protopost, seismic;

enum Protocol:
  case http, https;

object Location:
  val DefaultProtopost = Location(Protocol.http,"localhost", DefaultPort)
  val UrlRegex = """^(\w+)\:\/\/(localhost|(?:(?:[a-zA-Z_0-9]\.)+[a-zA-Z_0-9]+))(?:\:(\d+))?(?:\/)?$""".r
  def apply( putativeProtopostUrl : String ) : Location =
    putativeProtopostUrl match
      case UrlRegex( p, host, port ) =>
        val protocol = try Protocol.valueOf(p.toLowerCase) catch { case t : Throwable => throw new UnsupportedProtocol(p, t) }
        Location( protocol, host, if port.isEmpty then DefaultPort else port.toInt )
      case _ => throw new BadServiceUrl( s"'${putativeProtopostUrl}' is not a valid service URL, which should look like '<protocol>://<host>/' or '<protocol>://<host>:<port>/'" )
  case class WithPath( location : Location, path : String ):
    def toUrl : String = pathJoin( location.toUrl, path )
end Location
case class Location( protocol : Protocol, host : String, port : Int ):
  def toUrl : String =
    if port == Location.DefaultPort then s"${protocol}://${host}/" else s"${protocol}://${host}:${port}/"

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
    val publicKeyBase64Url = BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(publicKey).hex0x
    s"${service}:[ES256(P-256)]
  def toLocationWithIdentifier = s"${location.toUrl}#${}"


