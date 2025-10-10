package protopost.server.db

import zio.*

import java.io.InputStream
import java.sql.Connection
import javax.sql.DataSource
import java.time.Instant

import protopost.common.api.{Destination,PostDefinition,PostDefinitionUpdate,PosterNoAuth}
import protopost.common.{EmailAddress,PosterId,Protocol}
import protopost.server.{PostDefinitionRaw,PosterWithAuth,SeismicNodeWithId}
import protopost.server.exception.{ApparentBug,BadSeismicNodeId,UnknownPoster}

import com.mchange.rehash.*

import com.mchange.conveniences.string.*

import com.mchange.sc.sqlutil.*
import com.mchange.sc.zsqlutil.*
import protopost.server.exception.EmailIsAlreadyRegistered
import protopost.server.exception.UnacceptableContentType
import protopost.common.api.RetrievedPostRevision
import protopost.common.api.PostRevisionHistory

class PgDatabase( val SchemaManager : PgSchemaManager ):
  val Schema = SchemaManager.LatestSchema

  val AcceptableContentTypes = Set("text/plain","text/html","text/markdown")

  def allDestinations( conn : Connection ) : Set[Destination] =
    Schema.Join.selectAllDestinations( conn )
  def allPosters( conn : Connection ) : Set[PosterWithAuth] =
    Schema.Table.Poster.selectAll( conn )
  def destinationsByPosterId( posterId : PosterId)( conn : Connection ) : Set[Destination] =
    Schema.Join.selectDestinationsForPosterId( posterId )( conn )
  def destinationDefined( seismicNodeId : Int, name : String )( conn : Connection ) : Boolean =
    Schema.Table.Destination.defined( seismicNodeId, name )( conn )
  def fetchHashForPoster( posterId : PosterId )( conn : Connection ) : Option[BCryptHash] =
    Schema.Table.Poster.select( posterId )( conn ).map( _.auth )
  def grant( seismicNodeId : Int, destinationName : String, posterId : PosterId )( conn : Connection ) : Unit =
    Schema.Table.DestinationPoster.insert( seismicNodeId, destinationName, posterId )( conn )
  def identifierWithLocationForSeismicNodeId( seismicNodeId : Int )( conn : Connection ) : String =
    Schema.Table.SeismicNode.selectById( seismicNodeId )( conn ) match
      case Some( seismicNodeWithId ) => seismicNodeWithId.identifierWithLocation
      case None => throw new BadSeismicNodeId( s"Expected a seismic node to be defined in the database with ID $seismicNodeId. Did not find one." )
  def newDestination( seismicNodeId : Int, name : String, nickname : Option[String] )( conn : Connection ) : Destination =
    Schema.Table.Destination.insert( seismicNodeId, name, nickname )( conn )
    val sn =
      seismicNodeById( seismicNodeId )( conn ).getOrElse:
        throw new BadSeismicNodeId( s"Expected a seismic node to be defined in the database with ID $seismicNodeId. Did not find one." )
    Destination( sn.toApiSeismicNode, name, nickname )
  def newPost(
    destinationSeismicNodeId : Int,
    destinationName          : String,
    owner                    : PosterId,
    title                    : Option[String]  = None,
    postAnchor               : Option[String]  = None,
    sprout                   : Option[Boolean] = None,
    inReplyToSpecifier       : Option[String]  = None,
    authors                  : Seq[String] = Seq.empty
  )( conn : Connection ) : Int =
    val postId = Schema.Sequence.PostId.selectNext( conn )
    Schema.Table.Post.insert( postId, destinationSeismicNodeId, destinationName, owner, title, postAnchor, sprout, inReplyToSpecifier, false, None )( conn )
    if authors.nonEmpty then replaceAuthorsForPost( postId, authors )( conn )
    postId
  def newPostMedia(
    postId        : Int,
    mediaPath     : String,
    contentType   : Option[String],
    contentLength : Long,
    media         : InputStream
  )( conn : Connection ) =
    Schema.Table.PostMedia.insert(postId,mediaPath,contentType,contentLength,media)
  def newPostRevision(postId : Int, contentType : String, body : String)( conn : Connection ) : Option[Instant] =
    if !AcceptableContentTypes(contentType) then
      throw new UnacceptableContentType(s"'${contentType}' not supported, must be one of ${commaListOr(AcceptableContentTypes.toSeq)}")
    else
      val saveTime = Instant.now()
      val rowsInserted = Schema.Table.PostRevision.insertIfPostChanged(postId, saveTime, contentType, body)( conn )
      rowsInserted match
        case 0 => None
        case 1 => Some(saveTime)
        case n => throw new ApparentBug(s"insertIfPostChanged should insert 0 or 1 row, apparently inserted $n.")
  def newSeismicNode( algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int )( conn : Connection ) : Int =
    val newId = Schema.Sequence.SeismicNodeId.selectNext( conn )
    Schema.Table.SeismicNode.insert( newId, algcrv, pubkey, protocol, host, port )( conn )
    newId
  private def postDefinitionFromRaw( pdr : PostDefinitionRaw )( conn : Connection ) : PostDefinition =
    val seismicNode = seismicNodeById( pdr.destinationSeismicNodeId )( conn ).getOrElse( throw new ApparentBug("Seismic node ID we just look up in this transaction must exist by db constraint!") )
    val owner = posterById( pdr.owner )( conn ).getOrElse( throw new ApparentBug("Owner PosterID we just look up in this transaction must exist by db constraint!") )
    val authors = Schema.Table.PostAuthor.select( pdr.postId )( conn )
    val nickname = Schema.Table.Destination.nicknameForDefined( pdr.destinationSeismicNodeId, pdr.destinationName )( conn ) // this should be an already-defined destination!
    val destination = Destination( seismicNode.toApiSeismicNode, pdr.destinationName, nickname )
    PostDefinition( pdr.postId, destination, owner.toApiPosterNoAuth, pdr.title, pdr.postAnchor, pdr.sprout, pdr.inReplyToSpecifier, pdr.publicationAttempted, pdr.htmlPermalink, authors)
  def postDefinitionForId( id : Int )( conn : Connection ) : Option[PostDefinition] =
    Schema.Table.Post.select( id )( conn ).map( postDefinitionFromRaw(_)(conn) )
  def postDefinitionsForDestination( seismicNodeId : Int, destinationName : String )( conn : Connection ) : Set[PostDefinition] =
    Schema.Table.Post.selectByDestination( seismicNodeId, destinationName )( conn ).map( postDefinitionFromRaw(_)(conn) )
  def postDefinitionsForDestinationAndOwner( seismicNodeId : Int, destinationName : String, ownerId : PosterId )( conn : Connection ) : Set[PostDefinition] =
    Schema.Table.Post.selectByDestinationAndOwner( seismicNodeId, destinationName, ownerId )( conn ).map( postDefinitionFromRaw(_)(conn) )
  def postRevisionBySaveTime( id : Int, saveTime : Instant )( conn : Connection ) : Option[RetrievedPostRevision] =
    Schema.Table.PostRevision.select(id, saveTime)( conn )
  def postRevisionLatest( id : Int )( conn : Connection ) : Option[RetrievedPostRevision] =
    Schema.Table.PostRevision.selectLatest( id )( conn )
  def postRevisionHistory( id : Int )( conn : Connection ) : PostRevisionHistory =
    val seq = Schema.Table.PostRevision.selectRevisionHistoryForPost(id)(conn)
    PostRevisionHistory(id, seq)
  def posterForEmail( email : EmailAddress )( conn : Connection ) : Option[PosterWithAuth] =
    Schema.Table.Poster.selectPosterWithAuthByEmail( email )( conn )
  def posterById( id : PosterId )( conn : Connection ) : Option[PosterWithAuth] =
    Schema.Table.Poster.select( id )( conn )
  def postersBySeismicNodeIdDestinationName( snid : Int, destinationName : String )( conn : Connection ) : Set[PosterWithAuth] =
    Schema.Join.selectPostersBySeismicNodeIdDestinationName(snid,destinationName)(conn)
  def replaceAuthorsForPost( postId : Int, authors : Seq[String] )( conn : Connection ) : Unit =
    Schema.Table.PostAuthor.deleteByPost(postId)(conn)
    authors.zipWithIndex.map( ( author, placement ) => Schema.Table.PostAuthor.insert( postId, placement, author )(conn) )
  def seismicNodeByHostPort( host : String, port : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByHostPort( host, port )( conn )
  def seismicNodeById( seismicNodeId : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectById( seismicNodeId )( conn )
  def seismicNodeByAlgcrvPubkey( algcrv : String, pubkey : Array[Byte] )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByAlgcrvPubkey( algcrv, pubkey )( conn )
  def seismicNodeByComponents( algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
    Schema.Table.SeismicNode.selectByComponents( algcrv, pubkey, protocol, host, port )( conn )
  def updateHashForPoster( posterId : PosterId, hash : BCryptHash )( conn : Connection ) : Unit =
    Schema.Table.Poster.updateHash( posterId, hash )( conn )
  def updatePostDefinitionMain( 
    postId : Int,
    title : Option[String],
    postAnchor : Option[String],
    sprout : Option[Boolean],
    inReplyToSpecifier : Option[String]
  )( conn : Connection ) =
    Schema.Table.Post.updateMain(
      postId = postId,
      title = title,
      postAnchor = postAnchor,
      sprout = sprout,
      inReplyToSpecifier = inReplyToSpecifier
    )( conn )

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
      posterWithAuthByEmail(email)(ds).map( _.map( _.toApiPosterNoAuth ) )
    def destinationsForPosterEmail( email : EmailAddress )( ds : DataSource ) : Task[Set[Destination]] =
      withConnectionTransactional( ds ): conn =>
        posterForEmail(email)(conn) match
          case None => throw new UnknownPoster( s"User '${email}' is unknown." )
          case Some( pwa ) => Schema.Join.selectDestinationsForPosterId( pwa.id )( conn )
    def seismicNodesByHostPort( host : String, port : Int )( ds : DataSource ) : Task[Option[SeismicNodeWithId]] =
      withConnectionTransactional( ds )( Schema.Table.SeismicNode.selectByHostPort( host, port ) )
    def seismicNodesByAlgcrvPubkey( ds : DataSource )( algcrv : String, pubkey : Array[Byte] ) : Task[Option[SeismicNodeWithId]] =
      withConnectionTransactional( ds )( Schema.Table.SeismicNode.selectByAlgcrvPubkey( algcrv, pubkey ) )
  end txn
end PgDatabase
