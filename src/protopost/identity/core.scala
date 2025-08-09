package protopost.identity

import com.mchange.cryptoutil.{*,given}

import com.mchange.conveniences.string.*
import java.security.interfaces.ECPublicKey
import protopost.crypto.BouncyCastleSecp256r1
import java.security.interfaces.ECPrivateKey
import protopost.{BadIdentifierFormat,BadLocation,BadServiceUrl,UnknownAlgorithmOrCurve,UnsupportedProtocol}

enum Service:
  case protopost, seismic;

enum Protocol( val defaultPort : Int ):
  case http  extends Protocol(80)  // for testing only! auth credentials are sent "in the clear", so only https should be used in production
  case https extends Protocol(443)

object Location:
  val DefaultProtopost = Location.Simple(Protocol.http,"localhost",Protocol.http.defaultPort)
  val UrlRegex = """^(\w+)\:\/\/(localhost|(?:(?:[a-zA-Z_0-9]\.)+[a-zA-Z_0-9]+))(?:\:(\d+))?(\/.*)?$""".r
  def apply( putativeProtopostUrl : String ) : Location =
    putativeProtopostUrl match
      case UrlRegex( p, host, portStr, path ) =>
        val protocol = try Protocol.valueOf(p.toLowerCase) catch { case t : Throwable => throw new UnsupportedProtocol(p, t) }
        val port = if portStr.nullOrBlank then protocol.defaultPort else portStr.toInt
        if path.nullOrBlank || path.length == 1 then
          Location.Simple( protocol, host, port )
        else
          Location.WithPath( protocol, host, port, path.substring(1) )
      case _ => throw new BadServiceUrl( s"'${putativeProtopostUrl}' is not a valid service URL, which should look like '<protocol>://<host>/' or '<protocol>://<host>:<port>/'" )
  def assertSimple( location : Location ) : Location.Simple =
    location match
      case simple : Location.Simple => simple
      case _ => throw new BadLocation(s"Expected a simple location, host and maybe port, but no path. Found '${location.toUrl}'")
  case class Simple( protocol : Protocol, host : String, port : Int ) extends Location:
    lazy val toUrl : String = if port == protocol.defaultPort then s"${protocol}://${host}/" else s"${protocol}://${host}:${port}/"
  case class WithPath( protocol : Protocol, host : String, port : Int, path : String ) extends Location:
    lazy val simple = Simple( protocol, host, port )
    lazy val toUrl : String = pathJoin( simple.toUrl, path )
trait Location:
  def protocol : Protocol
  def host : String
  def port : Int
  def toUrl : String

object PublicIdentity:
  val IdentifierWithLocationRegex = """^([^\[]+)\[([^\]]+)\]([^\:]+)\:(.+)$""".r
  def fromIdentifierWithLocation( s : String ) : PublicIdentity[?] =
    s match
      case IdentifierWithLocationRegex( service, algocrv, pkey, location ) =>
        val l = Location.assertSimple(Location(location))
        val s = Service.valueOf(service)
        algocrv.toUpperCase match
          case "ES256(P-256)" =>
            val uncompressedFormatPublicKey = pkey.decodeHex
            ES256PublicIdentity(l, s, BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes(uncompressedFormatPublicKey))
          case _ => throw new UnknownAlgorithmOrCurve(algocrv)
      case _ => throw new BadIdentifierFormat(s)

trait PublicIdentity[KPUB]:
  def location  : Location.Simple
  def service   : Service
  def publicKey : KPUB
  def toIdentifier : String
  def toIdentifierWithLocation : String
  
trait LocalIdentity[KPVT,KPUB]:
  def location   : Location.Simple
  def service    : Service
  def privateKey : KPVT
  def publicKey  : KPUB
  def toPublicIdentity : PublicIdentity[KPUB]

case class ES256PublicIdentity(val location : Location.Simple, val service : Service, val publicKey : ECPublicKey) extends PublicIdentity[ECPublicKey]:
  def toIdentifier: String =
    val publicKeyHex = BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(publicKey).hex0x
    s"${service}[ES256(P-256)]${publicKeyHex}"
  def toIdentifierWithLocation = s"${this.toIdentifier}:${location.toUrl}"

case class ES256LocalIdentity(val location : Location.Simple, val service : Service, val privateKey : ECPrivateKey, val publicKey : ECPublicKey) extends LocalIdentity[ECPrivateKey,ECPublicKey]:
  lazy val toPublicIdentity : ES256PublicIdentity = ES256PublicIdentity(location, service, publicKey)


