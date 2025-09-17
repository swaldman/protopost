package protopost.server.identity

import com.mchange.conveniences.string.*

import protopost.{BadLocation,BadServiceUrl,ProtopostException,UnsupportedProtocol}
import protopost.common.Protocol

object Location:
  val DefaultApiLocalPort = 8025
  val DefaultProtopost = Location.Simple(Protocol.http,"localhost",Protocol.http.defaultPort)
  //val DefaultProtopost = Location.Simple(Protocol.http,"localhost",Protocol.http.defaultPort)
  val UrlRegex = """^(\w+)\:\/\/(localhost|(?:(?:[a-zA-Z_0-9]+\.)+[a-zA-Z_0-9]+))(?:\:(\d+))?(\/.*)?$""".r
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
