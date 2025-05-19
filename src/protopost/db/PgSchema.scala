package protopost.db

import java.sql.*

import scala.util.Using

import com.mchange.sc.sqlutil.*

object PgSchema:
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

  trait Base:
    def Version : Int
  object V0 extends Base: // contains unversioned schema only
    override val Version = 0
  object V1 extends Base:
    override val Version = 1
    object Table:
      object Destination extends Creatable:
        override val Create = "CREATE TABLE destination ( id INTEGER PRIMARY KEY, seismic_host VARCHAR(1024), seismic_port INTEGER, seismic_auth CHAR(60) )"
      end Destination
      object User extends Creatable:
        override val Create = "CREATE TABLE user ( id VARCHAR(256) PRIMARY KEY, full_name VARCHAR(2048), auth CHAR(60) )"
      end User
      object Post extends Creatable:
        override val Create =
          """|CREATE TABLE post (
             |  destination_id        INTEGER,
             |  post_id               VARCHAR(256),
             |  content_type          VARCHAR(128),
             |  title                 VARCHAR(1024),
             |  sprout                BOOLEAN,
             |  in_reply_to_href      VARCHAR(1024),
             |  in_reply_to_mime_type VARCHAR(128),
             |  in_reply_to_guid      VARCHAR(1024),
             |  PRIMARY KEY ( destination_id, post_id ),
             |  FOREIGN KEY(destination_id) REFERENCES destination(id)
             |)""".stripMargin
      end Post
      object PostAuthor extends Creatable:
        /*
         *  Note that though the types match, full_name is NOT a
         *  foreign key referencing user.full_name. As a convenience,
         *  we may default to assuming that users are (first) author,
         *  but users are free to collaborate or publish on behalf of others.
         *  So there can be multiple authors not associated with registered
         *  users.
         */
        override val Create =
          """|CREATE TABLE post_author (
             |  destination_id           INTEGER,
             |  post_id                  VARCHAR(256),
             |  order                    INTEGER,
             |  full_name                VARCHAR(2048),
             |  PRIMARY KEY ( destination_id, post_id, order ),
             |  FOREIGN KEY(destination_id, post_id) REFERENCES post(destination_id, post_id)
             |)""".stripMargin
      end PostAuthor
      object PostRevision extends Creatable:
        override val Create =
          """|CREATE TABLE post_revision (
             |  destination_id INTEGER,
             |  post_id        VARCHAR(256),
             |  save_time      TIMESTAMP,
             |  body           TEXT,
             |  PRIMARY KEY ( destination_id, post_id, save_time )
             |  FOREIGN KEY(destination_id, post_id) REFERENCES post(destination_id, post_id)
             |)""".stripMargin
      end PostRevision
      object PostHistory extends Creatable:
        override val Create =
          """|CREATE TABLE post_history (
             |  destination_id           INTEGER,
             |  post_id                  VARCHAR(256),
             |  update_time              TIMESTAMP,
             |  save_time                TIMESTAMP,
             |  major_update_description VARCHAR(2048),
             |  PRIMARY KEY ( destination_id, post_id, update_time )
             |  FOREIGN KEY(destination_id, post_id, save_time) REFERENCES post_revision(destination_id, post_id, save_time)
             |)""".stripMargin
      end PostHistory
    end Table
    object Sequence:
      object DestinationId extends Creatable:
        protected val Create = "CREATE SEQUENCE destination_seq AS INTEGER"
      end DestinationId
    end Sequence
  end V1
