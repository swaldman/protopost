package protopost.db

import java.sql.*

import scala.util.Using

import com.mchange.sc.sqlutil.*
import com.mchange.sc.sqlutil.migrate.{Schema,MetadataKey}

import protopost.*
import protopost.LoggingApi.*

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
      object Destination extends Creatable:
        override val Create = "CREATE TABLE destination ( id INTEGER PRIMARY KEY, seismic_host VARCHAR(1024), seismic_port INTEGER, seismic_auth CHAR(60) )"
      end Destination
      object Poster extends Creatable:
        override val Create = "CREATE TABLE poster ( id INTEGER PRIMARY KEY, email VARCHAR(1024), full_name VARCHAR(2048), auth CHAR(60) )"
        private val Insert = "INSERT INTO poster( id, email, full_name, auth ) VALUES ( ?, ?, ?, ? )"
        def insert( conn : Connection, id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash ) =
          Using.resource( conn.prepareStatement( Insert ) ): ps =>
            ps.setLong  (1, id.toInt)
            ps.setString(2, email.str)
            ps.setString(3, fullName)
            ps.setString(4, new String(auth.unsafeInternalArray))
            val rowsInserted = ps.executeUpdate()
            TRACE.log(s"Inserted into poster, seqnum ${id.toInt}, ${rowsInserted} rows inserted.")
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
             |  destination_id           INTEGER,
             |  post_id                  INTEGER,
             |  save_time                TIMESTAMP,
             |  update_time              TIMESTAMP,
             |  major_update_description VARCHAR(2048),
             |  update_confirmation_time TIMESTAMP,
             |  PRIMARY KEY ( destination_id, post_id, update_time ),
             |  FOREIGN KEY(destination_id, post_id, save_time) REFERENCES post_revision(destination_id, post_id, save_time)
             |)""".stripMargin
      end PostPublicationHistory
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
      object DestinationId extends Creatable:
        protected val Create = "CREATE SEQUENCE destination_id_seq AS INTEGER"
      end DestinationId
      object PosterId extends Creatable:
        protected val Create = "CREATE SEQUENCE poster_id_seq AS INTEGER"
        private val SelectNext = "SELECT nextval('poster_id_seq')"
        def selectNext( conn : Connection ) : protopost.db.PosterId =
          Using.resource( conn.prepareStatement(SelectNext) ): ps =>
            Using.resource( ps.executeQuery() ): rs =>
              uniqueResult("select-next-poster-id-seq", rs)( rs => protopost.db.PosterId( rs.getInt(1) ) )
      end PosterId
      object PostId extends Creatable:
        protected val Create = "CREATE SEQUENCE post_id_seq AS INTEGER"
      end PostId
    end Sequence
  end V1
