package protopost.db

import java.sql.*
import javax.sql.DataSource
import com.mchange.sc.sqlutil.migrate.*
import com.mchange.sc.zsqlutil.*
import com.mchange.sc.zsqlutil.zmigrate.*
import zio.*

import scala.util.Using

import protopost.{ExternalConfig,MissingConfig}
import protopost.LoggingApi.*

class PgSchemaManager( externalConfig : ExternalConfig ) extends ZMigratory.Postgres[PgSchema.V1.type], SelfLogging:
  override val LatestSchema      = PgSchema.V1
  override val DumpFileAppDbTag  = "protopost-db"
  override val MetadataTableName = PgSchema.Unversioned.Table.Metadata.Name

  override def getRunningAppVersionIfAvailable() : Option[String] = Some( protopost.BuildInfo.version )

  override def fetchMetadataValue( conn : Connection, key : MetadataKey )             : Option[String] = PgSchema.Unversioned.Table.Metadata.select( conn, key )
  override def insertMetadataKeys( conn : Connection, pairs : (MetadataKey,String)* ) : Unit           = pairs.foreach( ( mdkey, value ) => PgSchema.Unversioned.Table.Metadata.insert(conn, mdkey, value) )
  override def updateMetadataKeys( conn : Connection, pairs : (MetadataKey,String)* ) : Unit           = pairs.foreach( ( mdkey, value ) => PgSchema.Unversioned.Table.Metadata.update(conn, mdkey, value) )

  override def hasMetadataTable( conn : Connection ) : Boolean = Using.resource( conn.getMetaData().getTables(null,null,PgSchema.Unversioned.Table.Metadata.Name,null) )( _.next() )

  override def fetchDumpDir(conn: java.sql.Connection): zio.Task[Option[os.Path]] = ZIO.attemptBlocking:
    Some( os.Path( externalConfig.get( ExternalConfig.Key.DatabaseDumpDir ).getOrElse( throw new MissingConfig( s"Cannot find database dump dir; '${ExternalConfig.Key.DatabaseDumpDir}' not configured." ) ) ) )

  override def runDump( ds : DataSource, mbDbName : Option[String], dumpFile : os.Path ) : Task[Unit] = simpleLocalRunDump( ds, mbDbName, dumpFile )

  val TargetSchemaVersion = LatestSchema.Version

  override def upMigrate(ds: javax.sql.DataSource, from: Option[Int]): zio.Task[Unit] =
    def upMigrateFrom_New() : Task[Unit] =
      TRACE.log( "upMigrateFrom_New()" )
      withConnectionTransactional( ds ): conn =>
        PgSchema.Unversioned.Table.Metadata.create( conn )
        insertMetadataKeys(
          conn,
          (MetadataKey.SchemaVersion, "0"),
          (MetadataKey.CreatorAppVersion, BuildInfo.version)
        )
    def upMigrateFrom_0() : Task[Unit] =
      TRACE.log( "upMigrateFrom_0()" )
      withConnectionTransactional( ds ): conn =>
        Using.resource( conn.createStatement() ): stmt =>
          PgSchema.V1.Table.Destination.create( stmt )
          PgSchema.V1.Table.User.create( stmt )
          PgSchema.V1.Table.DestinationUser.create( stmt )
          PgSchema.V1.Table.Post.create( stmt )
          PgSchema.V1.Table.PostAuthor.create( stmt )
          PgSchema.V1.Table.PostRevision.create( stmt )
          PgSchema.V1.Table.PostPublicationHistory.create( stmt )
          PgSchema.V1.Table.PostMedia.create( stmt )
          PgSchema.V1.Sequence.DestinationId.create( stmt )
          PgSchema.V1.Sequence.PostId.create( stmt )
        updateMetadataKeys(
          conn,
          (MetadataKey.SchemaVersion, "1"),
          (MetadataKey.CreatorAppVersion, BuildInfo.version)
        )
    TRACE.log( s"upMigrate( from=${from} )" )
    from match
      case None => upMigrateFrom_New()
      case Some( 0 ) => upMigrateFrom_0()
      case Some( `TargetSchemaVersion` ) =>
        ZIO.fail( new CannotUpMigrate( s"Cannot upmigrate from current target DB version: V${TargetSchemaVersion}" ) )
      case Some( other ) =>
        ZIO.fail( new CannotUpMigrate( s"Cannot upmigrate from unknown DB version: V${other}" ) )
