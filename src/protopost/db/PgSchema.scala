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
import protopost.common.*
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
    object Table:
      object SeismicNode extends Creatable:
        override val Create =
          """|CREATE TABLE seismic_node (
             |  id INTEGER PRIMARY KEY,
             |  algcrv VARCHAR(32),
             |  pubkey bytea,
             |  protocol VARCHAR(16),
             |  host VARCHAR(256),
             |  port INTEGER,
             |  UNIQUE( algcrv, pubkey ),
             |  UNIQUE( host, port ),
             |  UNIQUE( algcrv, pubkey, protocol, host, port ) -- no need to create an index on these, the uniqueness constraint does it, see https://www.postgresql.org/docs/current/indexes-unique.html
             |)""".stripMargin
        val Insert = "INSERT INTO seismic_node( id, algcrv, pubkey, protocol, host, port ) VALUES ( ?, ?, ?, ?, ?, ? )"
        val SelectById = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE id = ?"
        val SelectByHostPort = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE host = ? AND port = ?"
        val SelectByAlgcrvPubkey = "SELECT id, algcrv, pubkey, protocol, host, port FROM seismic_node WHERE algcrv = ? AND pubkey = ?"
        val SelectIdByComponents = "SELECT id FROM seismic_node WHERE algcrv = ? AND pubkey = ? AND protocol = ? AND host = ? AND port = ?"
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
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-by-host-port")( extractSeismicNodeWithId) )
        def selectByAlgcrvPubkey( algcrv : String, pubkey : Array[Byte] )( conn : Connection ) : Option[SeismicNodeWithId] =
          Using.resource( conn.prepareStatement( SelectByAlgcrvPubkey ) ): ps =>
            ps.setString(1, algcrv)
            ps.setBytes(2, pubkey)
            Using.resource( ps.executeQuery() )( zeroOrOneResult("select-by-algcrv-pubkey")(extractSeismicNodeWithId) )
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
             |  seismic_node_id INTEGER,
             |  name VARCHAR(256),
             |  PRIMARY KEY (seismic_node_id, name),
             |  FOREIGN KEY (seismic_node_id) REFERENCES seismic_node(id)
             |)""".stripMargin
        val SelectDefined = "SELECT EXISTS(SELECT 1 FROM destination WHERE seismic_node_id=? AND name=?)"
        val Insert = "INSERT INTO destination(seismic_node_id, name) VALUES ( ?, ? )"
        def insert( seismicNodeId : Int, name : String )( conn : Connection ) : Unit =
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setInt( 1, seismicNodeId )
            ps.setString( 2, name )
            ps.executeUpdate()
        def defined( seismicNodeId : Int, name : String )( conn : Connection ) : Boolean =
          Using.resource( conn.prepareStatement( SelectDefined ) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, name)
            Using.resource( ps.executeQuery() )( uniqueResult( "destination-defined" )( _.getBoolean(1) ) )
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
             |  nickname         VARCHAR(256),
             |  UNIQUE ( poster_id, nickname ), -- each destination's nickname should be unique to each poster
             |  PRIMARY KEY ( seismic_node_id, destination_name, poster_id ),
             |  FOREIGN KEY ( seismic_node_id ) references seismic_node(id),
             |  FOREIGN KEY ( poster_id ) references poster(id)
             |)""".stripMargin
        val Insert = s"INSERT INTO destination_poster(seismic_node_id, destination_name, poster_id, nickname) VALUES (?,?,?,?)"
        def insert( seismicNodeId : Int, destinationName : String, posterId : PosterId, nickname : Option[String] )( conn : Connection ) =
          Using.resource( conn.prepareStatement(Insert) ): ps =>
            ps.setInt(1, seismicNodeId)
            ps.setString(2, destinationName)
            ps.setInt(3, PosterId.i(posterId))
            setStringOptional( ps, 4, Types.VARCHAR, nickname )
            ps.executeUpdate()
      end DestinationPoster
      object Post extends Creatable:
        override val Create =
          """|CREATE TABLE post (
             |  id                    INTEGER NOT NULL,
             |  seismic_node_id       INTEGER NOT NULL,
             |  destination_name      VARCHAR(256) NOT NULL,
             |  owner                 INTEGER NOT NULL,
             |  post_anchor           VARCHAR(256),
             |  title                 VARCHAR(1024),
             |  sprout                BOOLEAN,
             |  in_reply_to_href      VARCHAR(1024),
             |  in_reply_to_mime_type VARCHAR(128),
             |  in_reply_to_guid      VARCHAR(1024),
             |  content_type          VARCHAR(256),
             |  published_permalink   VARCHAR(1024),
             |  UNIQUE ( seismic_node_id, destination_name, post_anchor ), -- anchors should be unique within destinations
             |  PRIMARY KEY ( id ),
             |  FOREIGN KEY(owner) REFERENCES poster(id),
             |  FOREIGN KEY(seismic_node_id, destination_name) REFERENCES destination(seismic_node_id,name)
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
             |  post_id                  INTEGER,
             |  placement                INTEGER,
             |  full_name                VARCHAR(2048),
             |  PRIMARY KEY ( post_id, placement ),
             |  FOREIGN KEY(post_id) REFERENCES post(id)
             |)""".stripMargin
      end PostAuthor
      object PostRevision extends Creatable:
        override val Create =
          """|CREATE TABLE post_revision (
             |  post_id        INTEGER,
             |  save_time      TIMESTAMP,
             |  body           TEXT,
             |  PRIMARY KEY ( post_id, save_time ),
             |  FOREIGN KEY(post_id) REFERENCES post(id)
             |)""".stripMargin
      end PostRevision
      object PostPublicationHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_publication_history (
             |  post_id                   INTEGER,
             |  save_time                 TIMESTAMP,
             |  update_time               TIMESTAMP,
             |  major_update_description  VARCHAR(2048),
             |  update_confirmation_state VARCHAR(128),   -- usually a git commit id
             |  PRIMARY KEY ( post_id, update_time ),
             |  FOREIGN KEY(post_id, save_time) REFERENCES post_revision(post_id, save_time)
             |)""".stripMargin
      end PostPublicationHistory
      object PostDeleteHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_delete_history (
             |  post_id                   INTEGER,
             |  delete_time               TIMESTAMP,
             |  delete_confirmation_state VARCHAR(128),
             |  PRIMARY KEY ( post_id, delete_time ),
             |  FOREIGN KEY(post_id) REFERENCES post(id)
             |)""".stripMargin
      end PostDeleteHistory
      object PostUndeleteHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_undelete_history (
             |  post_id                     INTEGER,
             |  undelete_time               TIMESTAMP,
             |  undelete_confirmation_state VARCHAR(128),
             |  PRIMARY KEY ( post_id, undelete_time ),
             |  FOREIGN KEY(post_id) REFERENCES post(id)
             |)""".stripMargin
      end PostUndeleteHistory
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
        def selectNext( conn : Connection ) : protopost.PosterId = Sequence.selectNext( "poster_id_seq" )( protopost.PosterId.apply )( conn )
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
      val SelectAllDestinations =
        """|SELECT seismic_node.id, seismic_node.algcrv, seismic_node.pubkey, seismic_node.protocol, seismic_node.host, seismic_node.port, destination.name
           |FROM destination
           |INNER JOIN seismic_node ON destination.seismic_node_id = seismic_node.id""".stripMargin
      val SelectDestinationsForPosterId =
        """|SELECT seismic_node.id, seismic_node.algcrv, seismic_node.pubkey, seismic_node.protocol, seismic_node.host, seismic_node.port, destination.name
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
        val protocol = Protocol.valueOf(protocolStr)
        val apiSeismicNode = api.SeismicNode( snid, algcrv, pubkey.hex0x, protocol, host, port )
        Destination(apiSeismicNode, name)
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
