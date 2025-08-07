package protopost.identity

import protopost.{BadRestackUrl,UnknownRestackProtocol}

import com.mchange.cryptoutil.{*,given}

import com.mchange.conveniences.string.*
import java.security.interfaces.ECPublicKey
import protopost.crypto.BouncyCastleSecp256r1
import java.security.interfaces.ECPrivateKey

enum Protocol:
  case protopost, seismic;

object Location:
  val DefaultPort = 80
  val DefaultProtopost = Location(Protocol.protopost,"localhost", DefaultPort)
  val UrlRegex = """^(\w+)\:\/\/(localhost|(?:(?:[a-zA-Z_0-9]\.)+[a-zA-Z_0-9]+))(?:\:(\d+))?(?:\/)?$""".r
  def apply( putativeProtopostUrl : String ) : Location =
    putativeProtopostUrl match
      case UrlRegex( p, host, port ) =>
        val protocol = try Protocol.valueOf(p.toLowerCase) catch { case t : Throwable => throw new UnknownRestackProtocol(p, t) }
        Location( protocol, host, if port.isEmpty then DefaultPort else port.toInt )
      case _ => throw new BadRestackUrl( s"'${putativeProtopostUrl}' is not a valid restack URL, which should look like '<service>://<host>/' or '<service>://<host>:<port>/'" )
  case class WithPath( location : Location, path : String ):
    def toUrl : String = pathJoin( location.toUrl, path )
end Location
case class Location( protocol : Protocol, host : String, port : Int ):
  def toUrl : String =
    if port == Location.DefaultPort then s"${protocol}://${host}/" else s"${protocol}://${host}:${port}/"

case class LocalIdentity( location : Location, privateKey : ECPrivateKey, publicKey : ECPublicKey )

case class PublicIdentity( location : Location, publicKey : ECPublicKey ):
  def toIdentifier = s"${location.toUrl}#${BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(publicKey).hex0x}"


