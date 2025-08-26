package protopost.effectlib

import protopost.{BadIdentifierFormat,BadService,ProtoSeismicNode,UnknownSeismicNode}
import protopost.db.PgDatabase
import protopost.identity.*

import zio.*

import java.sql.Connection

import com.mchange.cryptoutil.{*,given}

private def ensureSeismic( publicIdentity : PublicIdentity[?] ) : Task[Unit] =
  if publicIdentity.service == Service.seismic then ZIO.unit
  else throw new BadService( s"Destinations must live on seismic, not ${publicIdentity.service} nodes, as in identifier provided '${publicIdentity.toIdentifierWithLocation}'." )

private def convertProtoSeismicNodeToPublicIdentity( psn : ProtoSeismicNode, acceptAdvertised : Boolean ) : Task[PublicIdentity[?]] =
  val unwrapped = ProtoSeismicNode.s(psn)
  ZIO.attempt(PublicIdentity.fromIdentifierWithLocationSimple(unwrapped)).catchSome:
    case e : BadIdentifierFormat =>
      if acceptAdvertised then
        ZIO.attemptBlocking( Location.Simple(unwrapped).findAdvertisedPublicIdentityForService( Service.seismic ) )
      else
        ZIO.fail(e)

private def mbCreateSeismicNode( psn : ProtoSeismicNode, pi : PublicIdentity[?], createInDatabase : Boolean )( db : PgDatabase, conn : Connection ) : Task[Int] =
  val loc = pi.location
  if createInDatabase then
    ZIO.attemptBlocking( db.newSeismicNode( pi.algcrv, pi.publicKeyBytes.unsafeUnwrap, loc.protocol, loc.host, loc.port )( conn ) )
  else
    ZIO.fail( new UnknownSeismicNode( s"'$psn' refers to a seismic node not yet part of any destination. Please create a destination that includes it." ) )

private def findSeismicNode( pi : PublicIdentity[?] )( db : PgDatabase, conn : Connection ) : Task[Option[Int]] =
  ZIO.attemptBlocking:
    val loc = pi.location
    db.seismicNodeByComponents( pi.algcrv, pi.publicKeyBytes.unsafeUnwrap, loc.protocol, loc.host, loc.port )(conn).map( _.id )

def encounterProtoSeismicNode( psn : ProtoSeismicNode, acceptAdvertised : Boolean, createInDatabase : Boolean )( db : PgDatabase, conn : Connection ) : Task[Int] =
  for
    pi   <- convertProtoSeismicNodeToPublicIdentity( psn, acceptAdvertised )
    _    <- ensureSeismic( pi )
    mbid <- findSeismicNode( pi )( db, conn )
    id   <- mbid.fold( mbCreateSeismicNode(psn,pi,createInDatabase)(db,conn) )( ZIO.succeed )
  yield id

