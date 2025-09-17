package protopost.identity

import scala.collection.immutable

import com.mchange.cryptoutil.{*, given}

import protopost.UnknownAlgorithmOrCurve
import protopost.common.{Protocol,Service}
import protopost.server.crypto.BouncyCastleSecp256r1

object Proto:
  object Identifier:
    val regexInnerString =
      val services = Service.values.mkString("|")
      raw"""($services)\[([^\]]+)\](?:0x)?([0-9a-f]+)"""
    val regex = s"""^${regexInnerString}$$""".r
  case class Identifier( service : Service, algcrv : String, publicKeyHex : String ) extends Proto:
    lazy val publicKeyBytes : immutable.ArraySeq[Byte] = immutable.ArraySeq.ofByte( publicKeyHex.decodeHex )
    def plus( location : Proto.Location ) : Proto.PublicIdentity =
      PublicIdentity( service, algcrv, publicKeyHex, location.protocol, location.host, location.port )

  object Location:
    val regexInnerString =
      val protocols = Protocol.values.mkString("|")
      raw"""($protocols)\:\/\/([^\:\/]+)(?:\:(\d+))?\/?"""
    val regex = s"""^${regexInnerString}$$""".r
  case class Location( protocol : Protocol, host : String, port : Int ) extends Proto:
    def complete : protopost.identity.Location.Simple = protopost.identity.Location.Simple( protocol, host, port )
    def plus( identifier : Proto.Identifier ) : Proto.PublicIdentity =
      PublicIdentity( identifier.service, identifier.algcrv, identifier.publicKeyHex, protocol, host, port )

  object PublicIdentity:
    val regex = raw"""^${Identifier.regexInnerString}\:${Location.regexInnerString}$$""".r
  case class PublicIdentity( service : Service, algcrv : String, publicKeyHex : String, protocol : Protocol, host : String, port : Int ) extends Proto:
    def complete : protopost.identity.PublicIdentity[?] =
      algcrv.toUpperCase match
        case protopost.identity.PublicIdentity.ES256.algcrv =>
          val uncompressedFormatPublicKey = publicKeyHex.decodeHex
          val publicKey = BouncyCastleSecp256r1.publicKeyFromUncompressedFormatBytes(uncompressedFormatPublicKey)
          protopost.identity.PublicIdentity.ES256(protopost.identity.Location.Simple(protocol,host,port),service,publicKey)
        case _ => throw new UnknownAlgorithmOrCurve(algcrv)

sealed trait Proto


