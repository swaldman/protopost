package protopost.server

import scala.collection.immutable

import protopost.common.{Protocol,Service}
import protopost.server.exception.{BadService,UnknownSeismicNode}
import protopost.server.identity.{Location,Proto,PublicIdentity}

import com.mchange.cryptoutil.{*,given}

object ProtoSeismicNode:
  sealed trait Located extends ProtoSeismicNode:
    def location : Location.Simple

  case class fromLocationSimple( location : Location.Simple ) extends ProtoSeismicNode, Located
  case class fromIdentifier( algcrv : String, publicKeyBytes : immutable.ArraySeq[Byte] ) extends ProtoSeismicNode:
    override def toString() : String =
      s"${Service.seismic}[$algcrv]${publicKeyBytes.hex0x}"

  case class fromPublicIdentity( publicIdentity : PublicIdentity[?] ) extends ProtoSeismicNode, Located:
    lazy val location = publicIdentity.location

  object fromLocalId:
    val regex = raw"""^(\d+)$$""".r
  case class fromLocalId( seismicNodeId : Int ) extends ProtoSeismicNode

  private def assertSeismic( service : String ) =
    if service != Service.seismic.toString
      then BadService("Required a seismic identifier, found service ${service}")

  def apply( str : String ) : ProtoSeismicNode =
    str match
      case fromLocalId.regex(n) =>
        fromLocalId(n.toInt)
      case Proto.Location.regex( protocol, host, port ) =>
        fromLocationSimple(Location.Simple(Protocol.valueOf(protocol),host,port.toInt))
      case Proto.Identifier.regex( service, algcrv, pubKeyHex ) =>
        assertSeismic(service)
        fromIdentifier( algcrv, pubKeyHex.decodeHexToSeq )
      case Proto.PublicIdentity.regex( service, algcrv, pubKeyHex, protocol, host, port ) =>
        assertSeismic(service)
        val proto = Proto.PublicIdentity( Service.valueOf(service), algcrv, pubKeyHex, Protocol.valueOf(protocol), host, port.toInt )
        fromPublicIdentity( proto.complete )
      case _ =>
        throw new UnknownSeismicNode(s"Cannot interpret ${str} as something that might identify a seismic node.")

sealed trait ProtoSeismicNode

