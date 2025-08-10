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
  def apply( url : String ) : Location =
    url match
      case UrlRegex( p, host, portStr, path ) =>
        val protocol = try Protocol.valueOf(p.toLowerCase) catch { case t : Throwable => throw new UnsupportedProtocol(p, t) }
        val port = if portStr.nullOrEmpty then protocol.defaultPort else portStr.toInt
        if path.nullOrEmpty || path.length == 1 then
          Location.Simple( protocol, host, port )
        else
          Location.WithPath( protocol, host, port, path.substring(1) )
      case _ => throw new BadServiceUrl( s"'${url}' is not a valid service URL, which should look like '<protocol>://<host>/<optional-path>' or '<protocol>://<host>:<port>/<optional-path>'" )
  object Simple:
    def assert( location : Location ) : Location.Simple =
      location match
        case simple : Location.Simple => simple
        case _ => throw new BadLocation(s"Expected a simple location, host and maybe port, but no path. Found '${location.toUrl}'")
    def apply( url : String ) : Location.Simple = assert( Location(url) )
  case class Simple( protocol : Protocol, host : String, port : Int ) extends Location:
    def simple = this
    lazy val toUrl : String = if port == protocol.defaultPort then s"${protocol}://${host}/" else s"${protocol}://${host}:${port}/"
  object WithPath:
    def assert( location : Location ) : Location.WithPath =
      location match
        case wp : Location.WithPath => wp
        case _ => throw new BadLocation(s"Expected a location with a path, host and maybe port and also a path. No path found in '${location.toUrl}'")
    def apply( url : String ) : Location.WithPath = assert( Location(url) )
  case class WithPath( protocol : Protocol, host : String, port : Int, path : String ) extends Location:
    lazy val simple = Simple( protocol, host, port )
    lazy val toUrl : String = pathJoin( simple.toUrl, path )
trait Location:
  def protocol : Protocol
  def host : String
  def port : Int
  def simple : Location.Simple
  def toUrl : String

object PublicIdentity:
  val IdentifierWithLocationRegex = """^([^\[]+)\[([^\]]+)\]([^\:]+)\:(.+)$""".r
  def fromIdentifierWithLocationAny( s : String ) : (PublicIdentity[?], Option[String]) =
    s match
      case IdentifierWithLocationRegex( service, algocrv, pkey, location ) =>
        val la = Location(location)
        val s = Service.valueOf(service)
        algocrv.toUpperCase match
          case "ES256(P-256)" =>
            val uncompressedFormatPublicKey = pkey.decodeHex
            val publicIdentity = ES256PublicIdentity(la.simple, s, BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes(uncompressedFormatPublicKey))
            val path =
              la match
                case lwp : Location.WithPath => Some(lwp.path)
                case ls  : Location.Simple     => None
            (publicIdentity, path)  
          case _ => throw new UnknownAlgorithmOrCurve(algocrv)
      case _ => throw new BadIdentifierFormat(s)
  def fromIdentifierWithLocationSimple( s : String ) : PublicIdentity[?] =
    fromIdentifierWithLocationAny( s ) match
      case ( pi, None ) => pi
      case _            => throw new BadIdentifierFormat("Expected a simple identifier, but found an identifier with a path component: " + s)
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


