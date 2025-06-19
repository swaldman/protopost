package protopost.main

import javax.sql.DataSource

import zio.*
import protopost.LoggingApi.*
import protopost.ExternalConfig
import protopost.db.PgSchemaManager

import com.mchange.sc.sqlutil.migrate.DbVersionStatus

object ConfiguredCommand extends SelfLogging:
  object DbInit extends ConfiguredCommand:
    def zcommand =
      for
        sm      <- ZIO.service[PgSchemaManager]
        ds      <- ZIO.service[DataSource]
        vstatus <- sm.dbVersionStatus(ds)
      yield
        vstatus match
          case DbVersionStatus.SchemaMetadataNotFound => // as expected, the database is not initialized
            sm.upMigrate( ds, from=None )
            0
          case DbVersionStatus.SchemaMetadataDisordered( message : String ) =>
            FATAL.log(s"Unexpected or broken schema: ${message}")
            10
          case DbVersionStatus.ConnectionFailed =>
            FATAL.log(s"Could not connect to the database.")
            11
          case _ =>
            INFO.log("The database appears already to be initialized.")
            0
    end zcommand

sealed trait ConfiguredCommand:
  def zcommand : ZIO[ExternalConfig & PgSchemaManager & DataSource, Throwable, Int]

