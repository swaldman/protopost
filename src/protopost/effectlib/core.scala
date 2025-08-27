package protopost.effectlib

import protopost.{AppResources,BadIdentifierFormat,BadService,ProtoSeismicNode,SeismicNodeWithId,UnknownSeismicNode}
import protopost.crypto.publicKeyBytesForPublicKey
import protopost.db.PgDatabase
import protopost.identity.*

import zio.*

import java.sql.Connection

import com.mchange.cryptoutil.{*,given}

def encounterProtoSeismicNode( psn : ProtoSeismicNode, acceptAdvertised : Boolean, createInDatabase : Boolean )( ar : AppResources, db : PgDatabase, conn : Connection ) : Task[Int] =
  psn match
    case ProtoSeismicNode.fromPublicIdentity(pi) =>
      if pi.service != Service.seismic then
        throw new BadService( s"protopost destinations must live on seismic, not ${pi.service} nodes, as in identifier provided '${pi.toIdentifierWithLocation}'." )
      ZIO.attemptBlocking:
        val loc = pi.location
        val mb = db.seismicNodeByComponents( pi.algcrv, pi.publicKeyBytes.unsafeUnwrap, loc.protocol, loc.host, loc.port )(conn)
        mb match
          case Some( seismicNodeWithId ) => seismicNodeWithId.id
          case None =>
            if createInDatabase then
              val out = db.newSeismicNode( pi.algcrv, pi.publicKeyBytes.unsafeUnwrap, loc.protocol, loc.host, loc.port )(conn)
              out
            else
              throw new UnknownSeismicNode(s"We know no destinations on seismic node ${pi.toIdentifierWithLocation}" )
    case ProtoSeismicNode.fromIdentifier( algcrv, publicKeyBytes ) =>
      ZIO.attemptBlocking:
        val mb = db.seismicNodeByAlgcrvPubkey( algcrv, publicKeyBytes.unsafeUnwrap )( conn )
        mb match
          case Some( seismicNodeWithId ) => seismicNodeWithId.id
          case None =>
            throw new UnknownSeismicNode(s"We know no destinations on seismic node [$algcrv]${publicKeyBytes.hex0x}." )
    case ProtoSeismicNode.fromLocationSimple( ls ) =>
      ZIO.attemptBlocking( () ).flatMap: _ =>
        val mb = db.seismicNodeByHostPort( ls.host, ls.port )( conn )
        mb match
          case Some( seismicNodeWithId ) =>
            if seismicNodeWithId.location.protocol != ls.protocol then
              throw new UnknownSeismicNode(s"Seismic node we know on ${ls.host}:${ls.port} uses ${seismicNodeWithId.location.protocol}, not ${ls.protocol}." )
            else
              ZIO.succeed(seismicNodeWithId.id)
          case None =>
            if acceptAdvertised then
              val (algcrv, publicKey) =
                val url = ls.toUrl
                ar.jwkProviders.get(ls, Service.seismic) match
                  case Some(jwk) =>
                    Option(jwk.getPublicKey()) match
                      case Some( publicKey ) =>
                        Option(jwk.getAlgorithm()) match
                          case Some(alg) =>
                            Option(jwk.getAdditionalAttributes().get("crv")) match
                              case Some(crv) =>
                                (s"$alg($crv)", publicKey)
                              case None =>
                                ( alg, publicKey )
                          case None =>
                            throw new BadIdentifierFormat(s"Could not lookup required from advertised JWKS on node '${url}'.")
                      case None =>
                        throw new BadIdentifierFormat(s"Could not lookup required public key from advertised JWKS on node '${url}'.")
                  case None =>
                    throw new BadIdentifierFormat("Could not find a seismic JWK advertised by node '${url}' to build identifier")
              end val
              val pubKeyBytes = publicKeyBytesForPublicKey( algcrv, publicKey )
              val pubKeyHex = pubKeyBytes.hex0x
              val pi = Proto.PublicIdentity( Service.seismic, algcrv, pubKeyHex, ls.protocol, ls.host, ls.port ).complete
              encounterProtoSeismicNode( ProtoSeismicNode.fromPublicIdentity(pi), acceptAdvertised, createInDatabase )( ar, db, conn )
            else
              ZIO.fail( new UnknownSeismicNode(s"We know no seismic node on ${ls.host}:${ls.port}, and check if such a node is advertised. (If you want to trust the node, set --accept-advertised to true.)" ) )


