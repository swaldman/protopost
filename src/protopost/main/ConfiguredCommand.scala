package protopost.main

import java.sql.Connection
import javax.sql.DataSource

import zio.*
import protopost.LoggingApi.*
import protopost.{ExternalConfig,ProtopostException}
import protopost.db.PgSchemaManager

import com.mchange.sc.sqlutil.migrate.DbVersionStatus

object ConfiguredCommand extends SelfLogging:
  object DbInit extends ConfiguredCommand:
    private def taskForStatus( sm : PgSchemaManager, conn : Connection, vstatus : DbVersionStatus ) : Task[Int] =
      vstatus match
        case DbVersionStatus.SchemaMetadataNotFound => // as expected, the database is not initialized
          for
            _ <- INFO.zlog("Initializing protopost database.")
            _ <- sm.migrate( conn )
          yield 0
        case DbVersionStatus.Current(_) =>
          INFO.zlog("The database is already initialized and up-to-date." ) *> ZIO.succeed(0)
        case other =>
          other.errMessage.fold( SEVERE.zlog( s"Could not initialize the database, status: ${vstatus}" ) )(msg => SEVERE.zlog(msg)) *> ZIO.succeed(10)
    def zcommand =
      for
        sm      <- ZIO.service[PgSchemaManager]
        ds      <- ZIO.service[DataSource]
        conn    =  ds.getConnection()
        vstatus <- sm.dbVersionStatus(conn)
        code    <- taskForStatus( sm, conn, vstatus )
      yield code
    end zcommand

sealed trait ConfiguredCommand:
  def zcommand : ZIO[ShutdownHooks & ExternalConfig & PgSchemaManager & DataSource, Throwable, Int]

