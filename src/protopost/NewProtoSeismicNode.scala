package protopost

import scala.collection.immutable

import protopost.identity.{Location,Proto,Protocol,PublicIdentity,Service}

import com.mchange.cryptoutil.{*,given}

object NewProtoSeismicNode:
  sealed trait Located extends NewProtoSeismicNode:
    def location : Location.Simple

  case class fromLocationSimple( location : Location.Simple ) extends NewProtoSeismicNode, Located
  case class fromIdentifier( algcrv : String, publicKeyBytes : immutable.ArraySeq[Byte] ) extends NewProtoSeismicNode

  case class fromPublicIdentity( publicIdentity : PublicIdentity[?] ) extends NewProtoSeismicNode, Located:
    lazy val location = publicIdentity.location

  object fromLocalId:
    val regex = raw"""^(\d+)$$""".r
  case class fromLocalId( seismicNodeId : Int ) extends NewProtoSeismicNode

  def apply( str : String ) : NewProtoSeismicNode =
    str match
      case fromLocalId.regex(n) =>
        fromLocalId(n.toInt)
      case Proto.Location.regex( protocol, host, port ) =>
        fromLocationSimple(Location.Simple(Protocol.valueOf(protocol),host,port.toInt))
      case Proto.Identifier.regex( service, algcrv, pubKeyHex ) =>
        val resolvedService = Service.valueOf(service)
        if resolvedService != Service.seismic then
          throw new BadService("Required a seismic identifier, found service ${resolvedService}")
        else
          fromIdentifier( algcrv, pubKeyHex.decodeHexToSeq )
      case _ => throw new BadSeismicNode(s"Cannot interpret ${str} as something that might identify a seismic node.")

sealed trait NewProtoSeismicNode

