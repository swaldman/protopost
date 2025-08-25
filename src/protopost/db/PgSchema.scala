package protopost.db

import java.sql.*
import scala.Array

import scala.collection.immutable
import scala.util.Using

import com.mchange.sc.sqlutil.*
import com.mchange.sc.sqlutil.migrate.{Schema,MetadataKey}

import com.mchange.rehash.*

import com.mchange.cryptoutil.given

import protopost.*
import protopost.api.*
import protopost.identity.*
import protopost.LoggingApi.*

import EmailAddress.{s => es}

object PgSchema extends SelfLogging:
  object Unversioned:
    object Table:
      object Metadata extends Creatable: // Creatable is now defined in sqlutil
        val Name = "metadata"
        protected val Create = "CREATE TABLE metadata( key VARCHAR(256) PRIMARY KEY, value VARCHAR(256) NOT NULL )"
        private val Insert = "INSERT INTO metadata(key, value) VALUES( ?, ? )"
        private val Update = "UPDATE metadata SET value = ? WHERE key = ?"
        private val Select = "SELECT value FROM metadata WHERE key = ?"
        def insert( conn : Connection, key : MetadataKey, value : String ) : Int =
          Using.resource( conn.prepareStatement( this.Insert ) ): ps =>
            ps.setString( 1, key.toString() )
            ps.setString( 2, value )
            ps.executeUpdate()
        def update( conn : Connection, key : MetadataKey, newValue : String ) : Int =
          Using.resource( conn.prepareStatement(this.Update) ): ps =>
            ps.setString(1, newValue)
            ps.setString(2, key.toString())
            ps.executeUpdate()
        def select( conn : Connection, key : MetadataKey ) : Option[String] =
          Using.resource( conn.prepareStatement( this.Select ) ): ps =>
            ps.setString(1, key.toString())
            Using.resource( ps.executeQuery() ): rs =>
              zeroOrOneResult("select-metadata", rs)( _.getString(1) )

  object V0 extends Schema: // contains unversioned schema only
    override val Version = 0
  object V1 extends Schema:
    override val Version = 1
    object Table:
      object SeismicNode extends Creatable:
        override val Create =
          """|CREATE TABLE seismic_node (
             |  id INTEGER PRIMARY KEY,
             |  algcrv VARCHAR(32),
             |  pubkey bytea,
             |  protocol VARCHAR(16),
             |  host VARCHAR(256),
             |  port INTEGER
             |)""".stripMargin
        val Insert = "INSERT INTO seismic_node( id, algcrv, pubkey, protocol, host, port ) VALUES ( ?, ?, ?, ?, ?, ? )"
        val SelectByHostPort = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE host = ? AND port = ?"
        val SelectByPubkey = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE pubkey = ?"
        def insert( conn : Connection, id : Int, algcrv : String, pubkey : Array[Byte], protocol : String, host : String, port : Int ) : Unit =
          Using.resource( conn.prepareStatement(Insert) ): ps =>
            ps.setInt(1, id)
            ps.setString(2, algcrv)
            ps.setBytes(3, pubkey)
            ps.setString(4, protocol)
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
        def selectByHostPort( conn : Connection, host : String, port : Int ) : Set[SeismicNodeWithId] =
          Using.resource( conn.prepareStatement( SelectByHostPort ) ): ps =>
            ps.setString(1, host)
            ps.setInt(2, port)
            Using.resource( ps.executeQuery() )( rs => toSet(rs)(extractSeismicNodeWithId) )
        def selectByPubkey( conn : Connection, pubkey : Array[Byte] ) : Set[SeismicNodeWithId] =
          Using.resource( conn.prepareStatement( SelectByPubkey ) ): ps =>
            ps.setBytes(1, pubkey)
            Using.resource( ps.executeQuery() )( rs => toSet(rs)(extractSeismicNodeWithId) )
      end SeismicNode
      object Destination extends Creatable:
        override val Create =
          """|CREATE TABLE destination (
             |  id INTEGER PRIMARY KEY,
             |  seismic_node_id INTEGER,
             |  name VARCHAR(256),
             |  FOREIGN KEY (seismic_node_id) REFERENCES seismic_node(id)
             |)""".stripMargin
        val Insert = "INSERT INTO destination(id, seismic_node_id, name) VALUES ( ?, ?, ? )"
        def insert( conn : Connection, id : Int, seismicNodeId : Int, name : String ) : Unit =
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setInt( 1, id )
            ps.setInt( 2, seismicNodeId )
            ps.setString( 3, name )
            ps.executeUpdate()
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
        private val SelectPosterWithAuthByEmail = "SELECT id, email, full_name, auth FROM poster WHERE email = ?"
        private val SelectPosterExistsForEmail = "SELECT EXISTS(SELECT 1 FROM poster WHERE email = ?)"
        private val UpdateHash = "UPDATE poster SET hash = ? WHERE id = ?"
        def updateHash( conn : Connection, posterId : PosterId, hash : BCryptHash ) : Unit =
          import com.mchange.rehash.str
          import PosterId.i
          val count =
            Using.resource( conn.prepareStatement( UpdateHash ) ): ps =>
              ps.setString(1, new String(hash.unsafeInternalArray))
              ps.setInt(2, i(posterId))
              ps.executeUpdate()
          if count == 0 then
            throw new PosterUnknown( s"Poster with ID ${posterId} not found." )
          else if count == 1 then
            ()
          else
            throw new InternalError( s"id is poster primary key, should reference zero or one row, found ${count}." )
        def select( conn : Connection, posterId : PosterId ) : Option[PosterWithAuth] =
          import PosterId.i
          Using.resource( conn.prepareStatement( Select ) ): ps =>
            ps.setInt(1, i(posterId))
            Using.resource( ps.executeQuery() ): rs =>
              zeroOrOneResult("select-poster", rs): rs =>
                PosterWithAuth(
                  PosterId( rs.getInt(1) ),
                  EmailAddress( rs.getString(2) ),
                  rs.getString(3),
                  BCryptHash( rs.getString(4).toCharArray )
                )
        def posterExistsForEmail( conn : Connection, email : EmailAddress ) : Boolean =
          Using.resource( conn.prepareStatement( SelectPosterExistsForEmail ) ): ps =>
            ps.setString(1, es(email) )
            Using.resource( ps.executeQuery() ): rs =>
              uniqueResult("poster-exists-for-email", rs)( _.getBoolean(1) )
        def selectPosterWithAuthByEmail( conn : Connection, email : EmailAddress ) : Option[PosterWithAuth] =
          Using.resource( conn.prepareStatement( SelectPosterWithAuthByEmail ) ): ps =>
            ps.setString(1, es(email))
            Using.resource( ps.executeQuery() ): rs =>
              zeroOrOneResult("poster-with-auth-by-email", rs): rs =>
                PosterWithAuth(
                  PosterId( rs.getInt(1) ),
                  EmailAddress( rs.getString(2) ),
                  rs.getString(3),
                  BCryptHash( rs.getString(4).toCharArray )
                )
        def insert( conn : Connection, id : PosterId, email : EmailAddress, fullName : String, auth : Option[BCryptHash] ) =
          import PosterId.i
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setLong  (1, i(id))
            ps.setString(2, es(email))
            ps.setString(3, fullName)
            auth match
              case Some( bch ) => ps.setString(4, new String(bch.unsafeInternalArray))
              case None        => ps.setNull(4, Types.CHAR)
            val rowsInserted = ps.executeUpdate()
            TRACE.log(s"Inserted into poster, seqnum ${i(id)}, ${rowsInserted} rows inserted.")
      end Poster
      object DestinationPoster extends Creatable:
        override val Create =
          """|CREATE TABLE destination_poster (
             |  destination_id INTEGER,
             |  poster_id      INTEGER,
             |  PRIMARY KEY ( destination_id, poster_id ),
             |  FOREIGN KEY ( destination_id ) references destination(id),
             |  FOREIGN KEY ( poster_id ) references poster(id)
             |)""".stripMargin
      end DestinationPoster
      object Post extends Creatable:
        override val Create =
          """|CREATE TABLE post (
             |  destination_id        INTEGER,
             |  post_id               INTEGER,
             |  post_anchor           VARCHAR(256),
             |  title                 VARCHAR(1024),
             |  sprout                BOOLEAN,
             |  in_reply_to_href      VARCHAR(1024),
             |  in_reply_to_mime_type VARCHAR(128),
             |  in_reply_to_guid      VARCHAR(1024),
             |  content_type          VARCHAR(256),
             |  published_permalink   VARCHAR(1024),
             |  UNIQUE ( destination_id, post_anchor ),
             |  PRIMARY KEY ( destination_id, post_id ),
             |  FOREIGN KEY(destination_id) REFERENCES destination(id)
             |)""".stripMargin
      end Post
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
             |  destination_id           INTEGER,
             |  post_id                  INTEGER,
             |  placement                INTEGER,
             |  full_name                VARCHAR(2048),
             |  PRIMARY KEY ( destination_id, post_id, placement ),
             |  FOREIGN KEY(destination_id, post_id) REFERENCES post(destination_id, post_id)
             |)""".stripMargin
      end PostAuthor
      object PostRevision extends Creatable:
        override val Create =
          """|CREATE TABLE post_revision (
             |  destination_id INTEGER,
             |  post_id        INTEGER,
             |  save_time      TIMESTAMP,
             |  body           TEXT,
             |  PRIMARY KEY ( destination_id, post_id, save_time ),
             |  FOREIGN KEY(destination_id, post_id) REFERENCES post(destination_id, post_id)
             |)""".stripMargin
      end PostRevision
      object PostPublicationHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_publication_history (
             |  destination_id            INTEGER,
             |  post_id                   INTEGER,
             |  save_time                 TIMESTAMP,
             |  update_time               TIMESTAMP,
             |  major_update_description  VARCHAR(2048),
             |  update_confirmation_state VARCHAR(128),               -- usually a git commit id
             |  PRIMARY KEY ( destination_id, post_id, update_time ),
             |  FOREIGN KEY(destination_id, post_id, save_time) REFERENCES post_revision(destination_id, post_id, save_time)
             |)""".stripMargin
      end PostPublicationHistory
      object PostDeleteHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_delete_history (
             |  destination_id            INTEGER,
             |  post_id                   INTEGER,
             |  delete_time               TIMESTAMP,
             |  delete_confirmation_state VARCHAR(128),
             |  PRIMARY KEY ( destination_id, post_id, delete_time )
             |)""".stripMargin
      end PostDeleteHistory
      object PostUndeleteHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_undelete_history (
             |  destination_id              INTEGER,
             |  post_id                     INTEGER,
             |  undelete_time               TIMESTAMP,
             |  undelete_confirmation_state VARCHAR(128),
             |  PRIMARY KEY ( destination_id, post_id, undelete_time )
             |)""".stripMargin
      end PostUndeleteHistory
      object PostMedia extends Creatable:
        override val Create =
          """|CREATE TABLE post_media (
             |  destination_id INTEGER,
             |  post_id        INTEGER,
             |  media_name     VARCHAR(1024),
             |  media          OID,
             |  PRIMARY KEY ( destination_id, post_id, media_name ),
             |  FOREIGN KEY(destination_id, post_id) references post(destination_id, post_id)
             |)""".stripMargin
      end PostMedia
    end Table
    object Sequence:
      private def selectNext[T]( conn : Connection, seqName : String )( wrap : Int => T) : T =
        Using.resource( conn.prepareStatement(s"SELECT nextval('${seqName}')") ): ps =>
          Using.resource( ps.executeQuery() ): rs =>
            uniqueResult(s"select-next-${seqName}", rs)( rs => wrap( rs.getInt(1) ) )
      object SeismicNodeId extends Creatable:
        protected val Create = "CREATE SEQUENCE seismic_node_id_seq AS INTEGER"
        def selectNext( conn : Connection ) : Int = Sequence.selectNext( conn, "seismic_node_id_seq" )( scala.Predef.identity )
      end SeismicNodeId
      object DestinationId extends Creatable:
        protected val Create = "CREATE SEQUENCE destination_id_seq AS INTEGER"
        def selectNext( conn : Connection ) : Int = Sequence.selectNext( conn, "destination_id_seq" )( scala.Predef.identity )
      end DestinationId
      object PosterId extends Creatable:
        protected val Create = "CREATE SEQUENCE poster_id_seq AS INTEGER"
        def selectNext( conn : Connection ) : protopost.PosterId = Sequence.selectNext( conn, "poster_id_seq" )( protopost.PosterId.apply )
      end PosterId
      object PostId extends Creatable:
        protected val Create = "CREATE SEQUENCE post_id_seq AS INTEGER"
      end PostId
    end Sequence
    object Index:
      object PosterEmail extends Creatable:
        protected val Create = "CREATE INDEX poster_email ON poster(email)"
      object PostPublishedPermalink extends Creatable:
        protected val Create = "CREATE INDEX post_published_permalink ON post(published_permalink)"
      object SeismicNodeByHostPort extends Creatable:
        protected val Create = "CREATE INDEX seismic_node_host_port ON seismic_node(host, port)"
      object SeismicNodeByPubkey extends Creatable:
        protected val Create = "CREATE INDEX seismic_node_pubkey ON seismic_node(pubkey)"
    end Index
    object Join:
      val SelectDestinationsForPosterId =
        """|SELECT seismic_node.algcrv, seismic_node.pubkey, seismic_node.protocol, seismic_node.host, seismic_node.port, destination.name
           |FROM destination_poster
           |INNER JOIN destination ON destination_poster.destination_id = destination.id
           |INNER JOIN seismic_node ON destination.seismic_node_id = seismic_node.id
           |WHERE destination_poster.id = ?""".stripMargin
      def selectDestinationsForPosterId( conn : Connection, posterId : PosterId ) : Set[ClientDestination] =
        val pid = PosterId.i(posterId)
        def extract( rs : ResultSet ) : ClientDestination =
          val algcrv      = rs.getString(1)
          val pubkey      = rs.getBytes(2)
          val protocolStr = rs.getString(3)
          val host        = rs.getString(4)
          val port        = rs.getInt(5)
          val name        = rs.getString(6)
          val service = Service.seismic
          val protocol = Protocol.valueOf(protocolStr)
          val portStr = if port == protocol.defaultPort then "" else s":${port}"
          val identifierWithLocation = s"${service}[${algcrv}]${pubkey.hex0x}:${protocol}://${host}${portStr}/"
          ClientDestination(pid, identifierWithLocation, name)
        Using.resource( conn.prepareStatement(SelectDestinationsForPosterId) ): ps =>
          ps.setInt(1, pid)
          Using.resource( ps.executeQuery() )( rs => toSet(rs)(extract) )
    end Join
  end V1
