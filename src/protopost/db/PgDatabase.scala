package protopost.db

import zio.*

import java.sql.Connection
import javax.sql.DataSource

import protopost.{BadSeismicNodeId,EmailAddress,PosterWithAuth,PosterId,PosterUnknown,SeismicNodeWithId}
import protopost.api.{Destination,PosterNoAuth}
import protopost.identity.Protocol

import com.mchange.rehash.*

import com.mchange.sc.sqlutil.*
import com.mchange.sc.zsqlutil.*
import protopost.EmailIsAlreadyRegistered

class PgDatabase( val SchemaManager : PgSchemaManager ):
  val Schema = SchemaManager.LatestSchema

  def allDestinations( conn : Connection ) : Set[Destination] =
    Schema.Join.selectAllDestinations( conn )
  def fetchHashForPoster( conn : Connection, posterId : PosterId ) : Option[BCryptHash] =
    Schema.Table.Poster.select( conn, posterId ).map( _.auth )
  def identifierWithLocationForSeismicNodeId( conn : Connection, seismicNodeId : Int ) : String =
    Schema.Table.SeismicNode.selectById( conn, seismicNodeId ) match
      case Some( seismicNodeWithId ) => seismicNodeWithId.identifierWithLocation
      case None => throw new BadSeismicNodeId( s"Expected a seismic node to be defined in the database with ID $seismicNodeId. Did not find one." )
  def newDestination( conn : Connection, seismicNodeId : Int, name : String ) : Destination =
    Schema.Table.Destination.insert( conn, seismicNodeId, name )
    Destination( identifierWithLocationForSeismicNodeId( conn, seismicNodeId ), name )
  def newSeismicNode( conn : Connection, algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int ) : Int =
    val newId = Schema.Sequence.SeismicNodeId.selectNext( conn )
    Schema.Table.SeismicNode.insert( conn, newId, algcrv, pubkey, protocol.toString, host, port )
    newId
  def updateHashForPoster( conn : Connection, posterId : PosterId, hash : BCryptHash ) : Unit =
    Schema.Table.Poster.updateHash( conn, posterId, hash )
  def posterWithAuthForEmail( conn : Connection, email : EmailAddress ) : Option[PosterWithAuth] =
    Schema.Table.Poster.selectPosterWithAuthByEmail( conn, email )
  def seismicNodesByHostPort( conn : Connection, host : String, port : Int ) : Set[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByHostPort( conn, host, port )
  def seismicNodesByPubkey( conn : Connection, pubkey : Array[Byte] ) : Set[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByPubkey( conn, pubkey )

  object txn:
    def createUser( ds : DataSource, authManager : AuthManager[PosterId] )( email : EmailAddress, fullName : String, password : Password ) : Task[PosterId] =
      withConnectionTransactional( ds ): conn =>
        val alreadyExists = Schema.Table.Poster.posterExistsForEmail( conn, email )
        if alreadyExists then
          throw new EmailIsAlreadyRegistered(s"Email '$email' is already registered by an existing user!")
        else
          val posterId = Schema.Sequence.PosterId.selectNext( conn )
          val auth = authManager.initialPasswordHash( posterId, password )
          Schema.Table.Poster.insert( conn, posterId, email, fullName, Some(auth) )
          posterId
    def posterWithAuthByEmail( ds : DataSource )( email : EmailAddress ) : Task[Option[PosterWithAuth]] =
      withConnectionTransactional( ds ): conn =>
        Schema.Table.Poster.selectPosterWithAuthByEmail(conn, email)
    def posterNoAuthByEmail( ds : DataSource )( email : EmailAddress ) : Task[Option[PosterNoAuth]] =
      posterWithAuthByEmail(ds)(email).map( _.map( _.toPosterNoAuth ) )
    def destinationsForPosterEmail( ds : DataSource )( email : EmailAddress ) : Task[Set[Destination]] =
      withConnectionTransactional( ds ): conn =>
        posterWithAuthForEmail(conn, email) match
          case None => throw new PosterUnknown( s"User '${email}' is unknown." )
          case Some( pwa ) => Schema.Join.selectDestinationsForPosterId( conn, pwa.id )
    def seismicNodesByHostPort( ds : DataSource )( host : String, port : Int ) : Task[Set[SeismicNodeWithId]] =
      withConnectionTransactional( ds )( conn => Schema.Table.SeismicNode.selectByHostPort( conn, host, port ) )
    def seismicNodesByPubkey( ds : DataSource )( pubkey : Array[Byte] ) : Task[Set[SeismicNodeWithId]] =
      withConnectionTransactional( ds )( conn => Schema.Table.SeismicNode.selectByPubkey( conn, pubkey ) )
  end txn
end PgDatabase
