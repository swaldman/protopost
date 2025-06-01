package protopost.db

import java.sql.*
import javax.sql.DataSource
import com.mchange.sc.sqlutil.migrate.*
import com.mchange.sc.zsqlutil.*
import com.mchange.sc.zsqlutil.zmigrate.*
import zio.*

import protopost.LoggingApi.*

object PgSchemaManager extends ZMigratory.Postgres[PgSchema.V1.type], SelfLogging:
  override val LatestSchema = PgSchema.V1

  override val AppDbTag = "protopost-db"

  override val MetadataTableName = PgSchema.Unversioned.Table.Metadata.Name

  override def fetchMetadataValue( conn : Connection, key : MetadataKey ) : Option[String] = PgSchema.Unversioned.Table.Metadata.select( conn, key )

  override def getRunningAppVersionIfAvailable(): Option[String] = Some( protopost.BuildInfo.version )

  //TODO
  override def fetchDumpDir(conn: java.sql.Connection): zio.Task[os.Path] = ???
  override def upMigrate(ds: javax.sql.DataSource, from: Option[Int]): zio.Task[Unit] = ???
