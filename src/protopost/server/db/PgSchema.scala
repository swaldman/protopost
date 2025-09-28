package protopost.server.db

import java.sql.*
import scala.Array

import scala.collection.immutable
import scala.util.Using

import com.mchange.sc.sqlutil.*
import com.mchange.sc.sqlutil.migrate.{Schema,MetadataKey}

import com.mchange.rehash.*

import com.mchange.cryptoutil.given

import protopost.common.api
import protopost.common.api.*
import protopost.common.{EmailAddress,PosterId,Protocol}
import protopost.server.{PosterWithAuth,PostDefinitionRaw,SeismicNodeWithId}
import protopost.server.exception.UnknownPoster
import protopost.server.identity.*
import protopost.server.LoggingApi.*

import EmailAddress.{s => es}
import protopost.server.db.PgSchema.V1.Table.Poster.posterExistsForEmail
import java.time.Instant

object PgSchema extends SelfLogging:
  object Unversioned:
    object Table:
      object Metadata extends Creatable: // Creatable is now defined in sqlutil
        val Name = "metadata"
        protected val Create = "CREATE TABLE metadata( key VARCHAR(256) PRIMARY KEY, value VARCHAR(256) NOT NULL )"
        private val Insert = "INSERT INTO metadata(key, value) VALUES( ?, ? )"
        private val Update = "UPDATE metadata SET value = ? WHERE key = ?"
        private val Select = "SELECT value FROM metadata WHERE key = ?"
        def insert( key : MetadataKey, value : String )( conn : Connection ) : Int =
          Using.resource( conn.prepareStatement( this.Insert ) ): ps =>
            ps.setString( 1, key.toString() )
            ps.setString( 2, value )
            ps.executeUpdate()
        def update( key : MetadataKey, newValue : String )( conn : Connection ) : Int =
          Using.resource( conn.prepareStatement(this.Update) ): ps =>
            ps.setString(1, newValue)
            ps.setString(2, key.toString())
            ps.executeUpdate()
        def select( key : MetadataKey )( conn : Connection ) : Option[String] =
          Using.resource( conn.prepareStatement( this.Select ) ): ps =>
            ps.setString(1, key.toString())
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-metadata")( _.getString(1) ) )

  object V0 extends Schema: // contains unversioned schema only
    override val Version = 0
  object V1 extends Schema:
    override val Version = 1
    object Type:
      object ProtocolType extends Creatable:
        override val Create = "CREATE TYPE protocol_type AS ENUM ('http', 'https')"
      object FeedKind extends Creatable:
        override val Create = "CREATE TYPE feed_kind AS ENUM ('rss', 'atom')"
      object FeedCurationType extends Creatable:
        override val Create = "CREATE TYPE feed_curation_type AS ENUM ('single', 'selection', 'all')"
      object PublicationEventType extends Creatable:
        override val Create = "CREATE TYPE publication_event_type AS ENUM ('publish', 'delete')"
    object Table:
      object SeismicNode extends Creatable:
        override val Create =
          """|CREATE TABLE seismic_node (
             |  id INTEGER PRIMARY KEY,
             |  algcrv VARCHAR(32),
             |  pubkey bytea,
             |  protocol protocol_type,
             |  host VARCHAR(256),
             |  port INTEGER,
             |  UNIQUE( algcrv, pubkey ),
             |  UNIQUE( host, port ),
             |  UNIQUE( algcrv, pubkey, protocol, host, port ) -- no need to create an index on these, the uniqueness constraint does it, see https://www.postgresql.org/docs/current/indexes-unique.html
             |)""".stripMargin
        val Insert = "INSERT INTO seismic_node( id, algcrv, pubkey, protocol, host, port ) VALUES ( ?, ?, ?, ?::protocol_type, ?, ? )"
        val SelectById = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE id = ?"
        val SelectByHostPort = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE host = ? AND port = ?"
        val SelectByAlgcrvPubkey = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE algcrv = ? AND pubkey = ?"
        val SelectIdByComponents = "SELECT id FROM seismic_node WHERE algcrv = ? AND pubkey = ? AND protocol = ?::protocol_type AND host = ? AND port = ?"
        def insert( id : Int, algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int )( conn : Connection ) =
          Using.resource( conn.prepareStatement(Insert) ): ps =>
            ps.setInt(1, id)
            ps.setString(2, algcrv)
            ps.setBytes(3, pubkey)
            ps.setString(4, protocol.toString)
            ps.setString(5, host)
            ps.setInt(6, port)
            ps.executeUpdate()
        private def extractSeismicNodeWithId( rs : ResultSet ) : SeismicNodeWithId =
          SeismicNodeWithId(
            rs.getInt(1),
            rs.getString(2),
            immutable.ArraySeq.ofByte(rs.getBytes(3)),
            Protocol.valueOf(rs.getString(4)),
            rs.getString(5),
            rs.getInt(6)
          )
        def selectByHostPort( host : String, port : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
          Using.resource( conn.prepareStatement( SelectByHostPort ) ): ps =>
            ps.setString(1, host)
            ps.setInt(2, port)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("seismic-node-by-host-port")(extractSeismicNodeWithId) )
        def selectByAlgcrvPubkey( algcrv : String, pubkey : Array[Byte] )( conn : Connection ) : Option[SeismicNodeWithId] =
          Using.resource( conn.prepareStatement( SelectByAlgcrvPubkey ) ): ps =>
            ps.setString(1, algcrv)
            ps.setBytes(2, pubkey)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("seismic-node-by-algcrv-pubkey")(extractSeismicNodeWithId) )
        def selectById( id : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
          Using.resource( conn.prepareStatement( SelectById ) ): ps =>
            ps.setInt(1, id)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("seismic-node-by-id")(extractSeismicNodeWithId) )
        def selectByComponents( algcrv : String, pubkey : Array[Byte], protocol : Protocol, host : String, port : Int )( conn : Connection ) : Option[SeismicNodeWithId] =
          def extract( rs : ResultSet ) : SeismicNodeWithId = SeismicNodeWithId( rs.getInt(1), algcrv, immutable.ArraySeq.ofByte(pubkey), protocol, host, port )
          Using.resource( conn.prepareStatement( SelectIdByComponents ) ): ps =>
            ps.setString(1, algcrv)
            ps.setBytes(2, pubkey)
            ps.setString(3, protocol.toString)
            ps.setString(4, host)
            ps.setInt(5, port)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("seismic-node-by-id")(extract) )
      end SeismicNode
      object Destination extends Creatable:
        override val Create =
          """|CREATE TABLE destination (
             |  seismic_node_id INTEGER      NOT NULL,
             |  name            VARCHAR(256) NOT NULL,
             |  nickname        VARCHAR(256),           -- can be NULL
             |  UNIQUE (nickname),
             |  PRIMARY KEY (seismic_node_id, name),
             |  FOREIGN KEY (seismic_node_id) REFERENCES seismic_node(id)
             |)""".stripMargin
        val SelectDefined = "SELECT EXISTS(SELECT 1 FROM destination WHERE seismic_node_id=? AND name=?)"
        val SelectNickname = "SELECT nickname FROM destination WHERE seismic_node_id = ? AND name = ?"
        val Insert = "INSERT INTO destination(seismic_node_id, name, nickname) VALUES ( ?, ?, ? )"
        def insert( seismicNodeId : Int, name : String, nickname : Option[String] )( conn : Connection ) : Unit =
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setInt( 1, seismicNodeId )
            ps.setString( 2, name )
            setStringOptional( ps, 3, Types.VARCHAR, nickname )
            ps.executeUpdate()
        def defined( seismicNodeId : Int, name : String )( conn : Connection ) : Boolean =
          Using.resource( conn.prepareStatement( SelectDefined ) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, name)
            Using.resource( ps.executeQuery() )( uniqueResult( "destination-defined" )( _.getBoolean(1) ) )
        def nicknameForDefined( seismicNodeId : Int, name : String )( conn : Connection ) : Option[String] =
          Using.resource( conn.prepareStatement( SelectDefined ) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, name)
            Using.resource( ps.executeQuery() )( uniqueResult( "nickname-for-defined" )( rs => Option(rs.getString(1)) ) )
        def nickname( seismicNodeId : Int, name : String )( conn : Connection ) : Option[Option[String]] =
          Using.resource( conn.prepareStatement( SelectDefined ) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, name)
            Using.resource( ps.executeQuery() )( zeroOrOneResult( "nickname" )( rs => Option(rs.getString(1)) ) )
      end Destination
      object Poster extends Creatable:
        override val Create =
          """|CREATE TABLE poster (
             |  id INTEGER PRIMARY KEY,
             |  email VARCHAR(1024),
             |  full_name VARCHAR(2048),
             |  auth CHAR(60),
             |  UNIQUE(email)
             |)""".stripMargin
        private val Insert = "INSERT INTO poster( id, email, full_name, auth ) VALUES ( ?, ?, ?, ? )"
        private val Select = "SELECT id, email, full_name, auth FROM poster WHERE id = ?"
        private val SelectAll = "SELECT id, email, full_name, auth FROM poster"
        private val SelectPosterWithAuthByEmail = "SELECT id, email, full_name, auth FROM poster WHERE email = ?"
        private val SelectPosterExistsForEmail = "SELECT EXISTS(SELECT 1 FROM poster WHERE email = ?)"
        private val UpdateHash = "UPDATE poster SET hash = ? WHERE id = ?"
        def updateHash( posterId : PosterId, hash : BCryptHash )( conn : Connection ) : Unit =
          import com.mchange.rehash.str
          import PosterId.i
          val count =
            Using.resource( conn.prepareStatement( UpdateHash ) ): ps =>
              ps.setString(1, new String(hash.unsafeInternalArray))
              ps.setInt(2, i(posterId))
              ps.executeUpdate()
          if count == 0 then
            throw new UnknownPoster( s"Poster with ID ${posterId} not found." )
          else if count == 1 then
            ()
          else
            throw new InternalError( s"id is poster primary key, should reference zero or one row, found ${count}." )
        private[V1] def extractPosterWithAuth( rs : ResultSet ) : PosterWithAuth =
          PosterWithAuth(
            PosterId( rs.getInt(1) ),
            EmailAddress( rs.getString(2) ),
            rs.getString(3),
            BCryptHash( rs.getString(4).toCharArray )
          )
        def select( posterId : PosterId )( conn : Connection ) : Option[PosterWithAuth] =
          import PosterId.i
          Using.resource( conn.prepareStatement( Select ) ): ps =>
            ps.setInt(1, i(posterId))
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-poster")(extractPosterWithAuth) )
        def selectAll( conn : Connection ) : Set[PosterWithAuth] =
          Using.resource( conn.prepareStatement( SelectAll ) ): ps =>
            Using.resource( ps.executeQuery() )( toSet(extractPosterWithAuth) )
        def posterExistsForEmail( email : EmailAddress )( conn : Connection ) : Boolean =
          Using.resource( conn.prepareStatement( SelectPosterExistsForEmail ) ): ps =>
            ps.setString(1, es(email) )
            Using.resource( ps.executeQuery() )( uniqueResult("poster-exists-for-email")( _.getBoolean(1) ) )
        def selectPosterWithAuthByEmail( email : EmailAddress )( conn : Connection ) : Option[PosterWithAuth] =
          Using.resource( conn.prepareStatement( SelectPosterWithAuthByEmail ) ): ps =>
            ps.setString(1, es(email))
            Using.resource( ps.executeQuery() )( zeroOrOneResult("poster-with-auth-by-email")(extractPosterWithAuth) )
        def insert( id : PosterId, email : EmailAddress, fullName : String, auth : Option[BCryptHash] )( conn : Connection ) =
          import PosterId.i
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setLong  (1, i(id))
            ps.setString(2, es(email))
            ps.setString(3, fullName)
            setStringOptional( ps, 4, Types.CHAR, auth.map(bch=>new String(bch.unsafeInternalArray)) )
            val rowsInserted = ps.executeUpdate()
            TRACE.log(s"Inserted into poster, seqnum ${i(id)}, ${rowsInserted} rows inserted.")
      end Poster
      object DestinationPoster extends Creatable:
        override val Create =
          """|CREATE TABLE destination_poster (
             |  seismic_node_id  INTEGER NOT NULL,
             |  destination_name VARCHAR(256) NOT NULL,
             |  poster_id        INTEGER NOT NULL,
             |  PRIMARY KEY ( seismic_node_id, destination_name, poster_id ),
             |  FOREIGN KEY ( seismic_node_id ) references seismic_node(id),
             |  FOREIGN KEY ( poster_id ) references poster(id)
             |)""".stripMargin
        val Insert = s"INSERT INTO destination_poster(seismic_node_id, destination_name, poster_id) VALUES (?,?,?)"
        def insert( seismicNodeId : Int, destinationName : String, posterId : PosterId )( conn : Connection ) =
          Using.resource( conn.prepareStatement(Insert) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, destinationName)
            ps.setInt(3, PosterId.i(posterId))
            ps.executeUpdate()
      end DestinationPoster
      object Post extends Creatable:
        override val Create =
          """|CREATE TABLE post (
             |  id                    INTEGER NOT NULL,
             |  seismic_node_id       INTEGER NOT NULL,
             |  destination_name      VARCHAR(256) NOT NULL,
             |  owner                 INTEGER NOT NULL,
             |  title                 VARCHAR(1024),
             |  post_anchor           VARCHAR(256),
             |  sprout                BOOLEAN,
             |  in_reply_to_href      VARCHAR(1024),
             |  in_reply_to_mime_type VARCHAR(128),
             |  in_reply_to_guid      VARCHAR(1024),
             |  publication_attempted BOOLEAN NOT NULL,
             |  html_permalink        VARCHAR(1024),
             |  UNIQUE ( seismic_node_id, destination_name, post_anchor ), -- anchors should be unique within destinations
             |  PRIMARY KEY ( id ),
             |  FOREIGN KEY(owner) REFERENCES poster(id),
             |  FOREIGN KEY(seismic_node_id, destination_name) REFERENCES destination(seismic_node_id,name)
             |)""".stripMargin
        val Select =
          """|SELECT id, seismic_node_id, destination_name, owner, title, post_anchor, sprout, in_reply_to_href, in_reply_to_mime_type, in_reply_to_guid, publication_attempted, html_permalink
             |FROM post
             |WHERE id = ?""".stripMargin
        val SelectByDestination =
          """|SELECT id, seismic_node_id, destination_name, owner, title, post_anchor, sprout, in_reply_to_href, in_reply_to_mime_type, in_reply_to_guid, publication_attempted, html_permalink
             |FROM post
             |WHERE seismic_node_id = ? AND destination_name = ?""".stripMargin
        val SelectByDestinationAndOwner =
          """|SELECT id, seismic_node_id, destination_name, owner, title, post_anchor, sprout, in_reply_to_href, in_reply_to_mime_type, in_reply_to_guid, publication_attempted, html_permalink
             |FROM post
             |WHERE seismic_node_id = ? AND destination_name = ? AND owner = ???""".stripMargin
        val Insert =
          """|INSERT INTO
             |post(id, seismic_node_id, destination_name, owner, title, post_anchor, sprout, in_reply_to_href, in_reply_to_mime_type, in_reply_to_guid, publication_attempted, html_permalink)
             |VALUES (?,?,?,?,?,?,?,?,?,?,?,?)""".stripMargin
        val UpdateMain =
          """|UPDATE post
             |SET title                 = ?,
             |    post_anchor           = ?,
             |    sprout                = ?,
             |    in_reply_to_href      = ?,
             |    in_reply_to_mime_type = ?,
             |    in_reply_to_guid      = ?
             |WHERE id = ?""".stripMargin
        private def extract( rs : ResultSet ) : PostDefinitionRaw =
          PostDefinitionRaw(
            postId = rs.getInt(1),
            destinationSeismicNodeId = rs.getInt(2),
            destinationName = rs.getString(3),
            owner = PosterId( rs.getInt(4) ),
            title = Option( rs.getString(5) ),
            postAnchor = Option( rs.getString(6) ),
            sprout = getBooleanOptionalAtPosition( rs, 7 ),
            inReplyToHref = Option( rs.getString(8) ),
            inReplyToMimeType = Option( rs.getString(9) ),
            inReplyToGuid = Option( rs.getString(10) ),
            publicationAttempted = rs.getBoolean(11),
            htmlPermalink = Option( rs.getString(12) )
          )
        def select( postId : Int )( conn : Connection ) : Option[PostDefinitionRaw] =
          Using.resource( conn.prepareStatement(Select) ): ps =>
            ps.setInt(1, postId)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-post-by-id")(extract) )
        def selectByDestination( seismicNodeId : Int, destinationName : String )( conn : Connection ) : Set[PostDefinitionRaw] =
          Using.resource( conn.prepareStatement(SelectByDestination) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, destinationName)
            Using.resource( ps.executeQuery() )( toSet(extract) )
        def selectByDestinationAndOwner( seismicNodeId : Int, destinationName : String, owner : PosterId )( conn : Connection ) : Set[PostDefinitionRaw] =
          Using.resource( conn.prepareStatement(SelectByDestinationAndOwner) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, destinationName)
            ps.setInt(3, PosterId.i(owner) )
            Using.resource( ps.executeQuery() )( toSet(extract) )
        def insert(
          newPostId                : Int,
          destinationSeismicNodeId : Int,
          destinationName          : String,
          owner                    : PosterId,
          title                    : Option[String],
          postAnchor               : Option[String],
          sprout                   : Option[Boolean],
          inReplyToHref            : Option[String],
          inReplyToMimeType        : Option[String],
          inReplyToGuid            : Option[String],
          publicationAttempted     : Boolean,
          htmlPermalink            : Option[String]
        )( conn : Connection ) =
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setInt(1, newPostId)
            ps.setInt(2, destinationSeismicNodeId)
            ps.setString(3, destinationName)
            ps.setInt(4, PosterId.i(owner))
            setStringOptional( ps, 5, Types.VARCHAR, title )
            setStringOptional( ps, 6, Types.VARCHAR, postAnchor ) 
            setBooleanOptional( ps, 7, sprout )
            setStringOptional( ps, 8, Types.VARCHAR, inReplyToHref )
            setStringOptional( ps, 9, Types.VARCHAR, inReplyToMimeType )
            setStringOptional( ps, 10, Types.VARCHAR, inReplyToGuid )
            ps.setBoolean( 11, publicationAttempted )
            setStringOptional( ps, 12, Types.VARCHAR, htmlPermalink )
            ps.executeUpdate()
        def updateMain(
          postId : Int,
          title : Option[String],
          postAnchor : Option[String],
          sprout : Option[Boolean],
          inReplyToHref : Option[String],
          inReplyToMimeType : Option[String],
          inReplyToGuid : Option[String]
        )( conn : Connection ) =
          Using.resource(conn.prepareStatement(UpdateMain)): ps =>
            setStringOptional( ps, 1, Types.VARCHAR, title )
            setStringOptional( ps, 2, Types.VARCHAR, postAnchor )
            setBooleanOptional( ps, 3, sprout )
            setStringOptional( ps, 4, Types.VARCHAR, inReplyToHref )
            setStringOptional( ps, 5, Types.VARCHAR, inReplyToMimeType )
            setStringOptional( ps, 6, Types.VARCHAR, inReplyToGuid )
            ps.setInt( 7, postId )
            ps.executeUpdate()
      end Post
      object FeedContainer extends Creatable:
        override val Create =
          """|CREATE TABLE feed_container (
             |  feed_href          VARCHAR(1024)      NOT NULL,
             |  kind               feed_kind          NOT NULL,
             |  container_curation feed_curation_type NOT NULL,
             |  PRIMARY KEY (feed_href)
             |)""".stripMargin
      object PostFeedContainer extends Creatable:
        override val Create =
          """|CREATE TABLE post_feed_container (
             |  post_id            INTEGER       NOT NULL,
             |  feed_href          VARCHAR(1024) NOT NULL,
             |  PRIMARY KEY (post_id, feed_href),
             |  FOREIGN KEY (post_id)   REFERENCES post(id),
             |  FOREIGN KEY (feed_href) REFERENCES feed_container(feed_href)
             |)""".stripMargin
      object PostFeedGuid extends Creatable:
        override val Create =
          """|CREATE TABLE post_feed_guid (
             |  post_id INTEGER       NOT NULL,
             |  guid    VARCHAR(1024) NOT NULL,
             |  PRIMARY KEY (post_id, guid),
             |  FOREIGN KEY (post_id) REFERENCES post(id)
             |)""".stripMargin
      object PostAuthor extends Creatable:
        /*
         *  Note that though the types match, full_name is NOT a
         *  foreign key referencing poster.full_name. As a convenience,
         *  we may default to assuming that posters are (first) author,
         *  but posters are free to collaborate or publish on behalf of others.
         *  So there can be multiple authors not associated with registered
         *  posters.
         */
        override val Create =
          """|CREATE TABLE post_author (
             |  post_id                  INTEGER,
             |  placement                INTEGER,
             |  full_name                VARCHAR(2048),
             |  PRIMARY KEY ( post_id, placement ),
             |  FOREIGN KEY(post_id) REFERENCES post(id)
             |)""".stripMargin
        val Select = "SELECT full_name FROM post_author WHERE post_id = ? ORDER BY placement"
        val DeleteByPost = "DELETE FROM post_author WHERE post_id = ?"
        val Insert = "INSERT INTO post_author( post_id, placement, full_name ) VALUES (?,?,?)"
        def select( postId : Int )( conn : Connection ) : Seq[String] =
          Using.resource( conn.prepareStatement( Select ) ): ps =>
            ps.setInt(1,postId)
            Using.resource( ps.executeQuery() )( toSeq( _.getString(1) ) )
        def deleteByPost( postId : Int )( conn : Connection ) =
          Using.resource( conn.prepareStatement( DeleteByPost ) ): ps =>
            ps.setInt(1,postId)
            ps.executeUpdate()
        def insert( postId : Int, placement : Int, fullName : String )( conn : Connection ) =
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setInt(1,postId)
            ps.setInt(2,placement)
            ps.setString(3,fullName)
            ps.executeUpdate()
      end PostAuthor
      object PostRevision extends Creatable:
        override val Create =
          """|CREATE TABLE post_revision (
             |  post_id        INTEGER NOT NULL,
             |  save_time      TIMESTAMP NOT NULL,
             |  content_type   VARCHAR(256) NOT NULL,
             |  body           TEXT,
             |  PRIMARY KEY ( post_id, save_time ),
             |  FOREIGN KEY(post_id) REFERENCES post(id)
             |)""".stripMargin
        val Select = "SELECT post_id, save_time, content_type, body FROM post_revision WHERE post_id = ? AND save_time = ?"
        val SelectLatest = "SELECT post_id, save_time, content_type, body FROM post_revision WHERE post_id = ? ORDER BY save_time DESC LIMIT 1" // InsertIfChanged and selectContentSameAsLatest are tightly coupled
        val SelectContentSameAsLatest = "SELECT latest.content_type = ? AND latest.body = ? from ( $SelectLatest ) AS latest"
        val Insert = "INSERT INTO post_revision(post_id, save_time, content_type, body) VALUES (?,?,?,?)"
        val InsertIfPostChanged = // note that first positional param will be post_id for SelectLatest!
          s"""|WITH latest_of_post AS (
              |  ${SelectLatest}
              |)
              |INSERT INTO post_revision(post_id, save_time, content_type, body)
              |SELECT ?, ?, ?, ?
              |WHERE NOT EXISTS (
              |  SELECT 1 FROM latest_of_post WHERE content_type = ? AND body = ?
              |)""".stripMargin
        private def extract( rs : ResultSet ) : RetrievedPostRevision =
          val postId      = rs.getInt(1)
          val ts          = rs.getTimestamp(2).toInstant
          val contentType = rs.getString(3)
          val body        = rs.getString(4)
          RetrievedPostRevision( postId, ts.getEpochSecond(), ts.getNano(), contentType, body )
        def insert( postId : Int, saveTime : Instant, contentType : String, body : String )( conn : Connection ) =
          Using.resource( conn.prepareStatement(Insert) ): ps =>
            ps.setInt(1, postId)
            ps.setTimestamp(2, Timestamp.from(saveTime))
            ps.setString(3, contentType)
            ps.setString(4, body)
            ps.executeUpdate()
        def insertIfPostChanged( postId : Int, saveTime : Instant, contentType : String, body : String )( conn : Connection ) =
          Using.resource( conn.prepareStatement(InsertIfPostChanged) ): ps =>
            ps.setInt(1, postId) // from SelectLatest!
            ps.setInt(2, postId)
            ps.setTimestamp(3, Timestamp.from(saveTime))
            ps.setString(4, contentType)
            ps.setString(5, body)
            ps.setString(6, contentType)
            ps.setString(7, body)
            ps.executeUpdate()
        def select( postId : Int, instant : Instant )( conn : Connection ) : Option[RetrievedPostRevision] =
          Using.resource( conn.prepareStatement(Select) ): ps =>
            ps.setInt(1, postId)
            ps.setTimestamp(2, Timestamp.from(instant))
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-post-revision")(extract) )
        def selectLatest( postId : Int )( conn : Connection ) : Option[RetrievedPostRevision] =
          Using.resource( conn.prepareStatement(SelectLatest) ): ps =>
            ps.setInt(1, postId)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-latest-post-revision")(extract) )
        def selectContentSameAsLatest( postId : Int, contentType : String, body : String )( conn : Connection ) : Option[RetrievedPostRevision] =
          Using.resource( conn.prepareStatement(SelectContentSameAsLatest) ): ps =>
            ps.setString(1, contentType)
            ps.setString(2, body)
            ps.setInt(3, postId) // from SelectLatest
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-revision-content-same-as-latest")(extract) )
      end PostRevision
      object PostPublicationHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_publication_history (
             |  post_id                   INTEGER,
             |  save_time                 TIMESTAMP,
             |  update_time               TIMESTAMP,
             |  publication_event         publication_event_type,
             |  major_update_description  VARCHAR(2048),
             |  update_confirmation_state VARCHAR(128),   -- usually a git commit id
             |  PRIMARY KEY ( post_id, update_time ),
             |  FOREIGN KEY(post_id, save_time) REFERENCES post_revision(post_id, save_time)
             |)""".stripMargin
      end PostPublicationHistory
      object PostMedia extends Creatable:
        override val Create =
          """|CREATE TABLE post_media (
             |  post_id        INTEGER,
             |  media_name     VARCHAR(1024),
             |  media          OID,
             |  PRIMARY KEY ( post_id, media_name ),
             |  FOREIGN KEY(post_id) references post(id)
             |)""".stripMargin
      end PostMedia
    end Table
    object Sequence:
      private def selectNext[T]( seqName : String )( wrap : Int => T )( conn : Connection )  : T =
        Using.resource( conn.prepareStatement(s"SELECT nextval('${seqName}')") ): ps =>
          Using.resource( ps.executeQuery() )( uniqueResult(s"select-next-${seqName}")( rs => wrap( rs.getInt(1) ) ) )
      object SeismicNodeId extends Creatable:
        protected val Create = "CREATE SEQUENCE seismic_node_id_seq AS INTEGER"
        def selectNext( conn : Connection ) : Int = Sequence.selectNext( "seismic_node_id_seq" )( scala.Predef.identity )( conn )
      end SeismicNodeId
      object PosterId extends Creatable:
        protected val Create = "CREATE SEQUENCE poster_id_seq AS INTEGER"
        def selectNext( conn : Connection ) : protopost.common.PosterId = Sequence.selectNext( "poster_id_seq" )( protopost.common.PosterId.apply )( conn )
      end PosterId
      object PostId extends Creatable:
        protected val Create = "CREATE SEQUENCE post_id_seq AS INTEGER"
        def selectNext( conn : Connection ) : Int = Sequence.selectNext( "post_id_seq" )( scala.Predef.identity )( conn )
      end PostId
    end Sequence
    object Index:
      object PostFeedGuidIndex extends Creatable:
        protected val Create = "CREATE INDEX post_feed_guid_index ON post_feed_guid(guid)"
      object PostRevisionSaveTimeIndex extends Creatable:
        protected val Create = "CREATE INDEX post_revision_save_time_index ON post_revision(save_time)"
      object PostRevisionBodyIndex extends Creatable:
        protected val Create = "CREATE INDEX post_revision_body_index ON post_revision(save_time) USING hash"
    end Index
    object Join:
      val SelectAllDestinations =
        """|SELECT seismic_node.id, seismic_node.algcrv, seismic_node.pubkey, seismic_node.protocol, seismic_node.host, seismic_node.port, destination.name, destination.nickname
           |FROM destination
           |INNER JOIN seismic_node ON destination.seismic_node_id = seismic_node.id""".stripMargin
      val SelectDestinationsForPosterId =
        """|SELECT seismic_node.id, seismic_node.algcrv, seismic_node.pubkey, seismic_node.protocol, seismic_node.host, seismic_node.port, destination.name, destination.nickname
           |FROM destination_poster
           |INNER JOIN destination ON destination_poster.seismic_node_id = destination.seismic_node_id AND destination_poster.destination_name = destination_name
           |INNER JOIN seismic_node ON destination.seismic_node_id = seismic_node.id
           |WHERE destination_poster.poster_id = ?""".stripMargin
      val SelectPostersBySeismicNodeIdDestinationName =
        """|SELECT poster.id, poster.email, poster.full_name, poster.auth
           |FROM poster
           |INNER JOIN destination_poster ON poster.id = destination_poster.poster_id
           |WHERE destination_poster.seismic_node_id = ? AND destination_poster.destination_name = ?""".stripMargin
      private def extractDestination( rs : ResultSet ) : Destination =
        val snid        = rs.getInt(1)
        val algcrv      = rs.getString(2)
        val pubkey      = rs.getBytes(3)
        val protocolStr = rs.getString(4)
        val host        = rs.getString(5)
        val port        = rs.getInt(6)
        val name        = rs.getString(7)
        val nickname    = Option(rs.getString(8))
        val protocol = Protocol.valueOf(protocolStr)
        val apiSeismicNode = api.SeismicNode( snid, algcrv, pubkey.hex0x, protocol, host, port )
        Destination(apiSeismicNode, name, nickname)
      def selectAllDestinations( conn : Connection ) : Set[Destination] =
        Using.resource( conn.prepareStatement(SelectAllDestinations) ): ps =>
          Using.resource( ps.executeQuery() )( toSet(extractDestination) )
      def selectDestinationsForPosterId( posterId : PosterId )( conn : Connection ) : Set[Destination] =
        val pid = PosterId.i(posterId)
        Using.resource( conn.prepareStatement(SelectDestinationsForPosterId) ): ps =>
          ps.setInt(1, pid)
          Using.resource( ps.executeQuery() )( toSet(extractDestination) )
      def selectPostersBySeismicNodeIdDestinationName( snid : Int, destinationName : String )( conn : Connection ) : Set[PosterWithAuth] =
        Using.resource( conn.prepareStatement(SelectPostersBySeismicNodeIdDestinationName) ): ps =>
          ps.setInt(1, snid)
          ps.setString(2, destinationName )
          Using.resource( ps.executeQuery() )( toSet( Table.Poster.extractPosterWithAuth ) )
    end Join
  end V1
