package protopost.db

import zio.*

import java.sql.Connection
import javax.sql.DataSource

import protopost.{BadSeismicNodeId,EmailAddress,PosterWithAuth,PosterId,SeismicNodeWithId,UnknownPoster}
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
  def destinationDefined( seismicNodeId : Int, name : String )( conn : Connection ) : Boolean =
    Schema.Table.Destination.defined( seismicNodeId, name )( conn )
  def fetchHashForPoster( posterId : PosterId )( conn : Connection ) : Option[BCryptHash] =
    Schema.Table.Poster.select( posterId )( conn ).map( _.auth )
  def identifierWithLocationForSeismicNodeId( seismicNodeId : Int )( conn : Connection ) : String =
    Schema.Table.SeismicNode.selectById( seismicNodeId )( conn ) match
      case Some( seismicNodeWithId ) => seismicNodeWithId.identifierWithLocation
      case None => throw new BadSeismicNodeId( s"Expected a seismic node to be defined in the database with ID $seismicNodeId. Did not find one." )
  def newDestination( seismicNodeId : Int, name : String )( conn : Connection ) : Destination =
    Schema.Table.Destination.insert( seismicNodeId, name )( conn )
    Destination( identifierWithLocationForSeismicNodeId( seismicNodeId )( conn ), name )
  def newSeismicNode( algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int )( conn : Connection ) : Int =
    val newId = Schema.Sequence.SeismicNodeId.selectNext( conn )
    Schema.Table.SeismicNode.insert( newId, algcrv, pubkey, protocol, host, port )( conn )
    newId
  def updateHashForPoster( posterId : PosterId, hash : BCryptHash )( conn : Connection ) : Unit =
    Schema.Table.Poster.updateHash( posterId, hash )( conn )
  def posterWithAuthForEmail( email : EmailAddress )( conn : Connection ) : Option[PosterWithAuth] =
    Schema.Table.Poster.selectPosterWithAuthByEmail( email )( conn )
  def seismicNodeByHostPort( host : String, port : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByHostPort( host, port )( conn )
  def seismicNodeByAlgcrvPubkey( pubkey : Array[Byte] )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByAlgcrvPubkey( pubkey )( conn )
  def seismicNodeByComponents( algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByComponents( algcrv, pubkey, protocol, host, port )( conn )

  object txn:
    def createUser( authManager : AuthManager[PosterId] )( email : EmailAddress, fullName : String, password : Password )( ds : DataSource ) : Task[PosterId] =
      withConnectionTransactional( ds ): conn =>
        val alreadyExists = Schema.Table.Poster.posterExistsForEmail( email )( conn )
        if alreadyExists then
          throw new EmailIsAlreadyRegistered(s"Email '$email' is already registered by an existing user!")
        else
          val posterId = Schema.Sequence.PosterId.selectNext( conn )
          val auth = authManager.initialPasswordHash( posterId, password )
          Schema.Table.Poster.insert( posterId, email, fullName, Some(auth) )( conn )
          posterId
    def posterWithAuthByEmail( email : EmailAddress )( ds : DataSource ) : Task[Option[PosterWithAuth]] =
      withConnectionTransactional( ds ): conn =>
        Schema.Table.Poster.selectPosterWithAuthByEmail(email)( conn )
    def posterNoAuthByEmail( email : EmailAddress )( ds : DataSource ) : Task[Option[PosterNoAuth]] =
      posterWithAuthByEmail(email)(ds).map( _.map( _.toPosterNoAuth ) )
    def destinationsForPosterEmail( email : EmailAddress )( ds : DataSource ) : Task[Set[Destination]] =
      withConnectionTransactional( ds ): conn =>
        posterWithAuthForEmail(email)(conn) match
          case None => throw new UnknownPoster( s"User '${email}' is unknown." )
          case Some( pwa ) => Schema.Join.selectDestinationsForPosterId( pwa.id )( conn )
    def seismicNodesByHostPort( host : String, port : Int )( ds : DataSource ) : Task[Option[SeismicNodeWithId]] =
      withConnectionTransactional( ds )( Schema.Table.SeismicNode.selectByHostPort( host, port ) )
    def seismicNodesByAlgcrvPubkey( ds : DataSource )( pubkey : Array[Byte] ) : Task[Option[SeismicNodeWithId]] =
      withConnectionTransactional( ds )( Schema.Table.SeismicNode.selectByAlgcrvPubkey( pubkey ) )
  end txn
end PgDatabase
