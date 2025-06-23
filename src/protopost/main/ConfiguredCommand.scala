package protopost.main

import javax.sql.DataSource

import zio.*
import protopost.LoggingApi.*
import protopost.ExternalConfig
import protopost.db.PgSchemaManager

import com.mchange.sc.sqlutil.migrate.DbVersionStatus

object ConfiguredCommand extends SelfLogging:
  object DbInit extends ConfiguredCommand:
    private def taskForStatus( sm : PgSchemaManager, ds : DataSource, vstatus : DbVersionStatus ) : Task[Int] =
      vstatus match
        case DbVersionStatus.SchemaMetadataNotFound => // as expected, the database is not initialized
          for
            _ <- INFO.zlog("Initializing protopost database.")
            _ <- sm.upMigrate( ds, from=None )
          yield 0
        case DbVersionStatus.SchemaMetadataDisordered( message : String ) =>
          for
            _ <- FATAL.zlog(s"Unexpected or broken schema: ${message}")
          yield 10
        case DbVersionStatus.ConnectionFailed =>
          for
            _ <- FATAL.zlog(s"Could not connect to the database.")
          yield 11
        case _ =>
          for
            _ <- INFO.zlog("The database appears already to be initialized.")
          yield 0
    def zcommand =
      for
        sm      <- ZIO.service[PgSchemaManager]
        ds      <- ZIO.service[DataSource]
        vstatus <- sm.dbVersionStatus(ds)
        code    <- taskForStatus( sm, ds, vstatus )
      yield code
    end zcommand

sealed trait ConfiguredCommand:
  def zcommand : ZIO[ShutdownHooks & ExternalConfig & PgSchemaManager & DataSource, Throwable, Int]

