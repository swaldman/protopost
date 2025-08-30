package protopost.db

import java.sql.*
import javax.sql.DataSource
import com.mchange.sc.sqlutil.migrate.*
import com.mchange.sc.zsqlutil.*
import com.mchange.sc.zsqlutil.zmigrate.*
import zio.*

import scala.util.Using

import protopost.{BuildInfo,ExternalConfig,MissingConfig}
import protopost.LoggingApi.*

class PgSchemaManager( externalConfig : ExternalConfig ) extends ZMigratory.Postgres[PgSchema.V1.type], SelfLogging:
  override val LatestSchema      = PgSchema.V1
  override val DumpFileAppDbTag  = "protopost-db"
  override val MetadataTableName = PgSchema.Unversioned.Table.Metadata.Name

  override def getRunningAppVersionIfAvailable() : Option[String] = Some( protopost.BuildInfo.version )

  override def fetchMetadataValue( conn : Connection, key : MetadataKey )             : Option[String] = PgSchema.Unversioned.Table.Metadata.select( key )( conn )
  override def insertMetadataKeys( conn : Connection, pairs : (MetadataKey,String)* ) : Unit           = pairs.foreach( ( mdkey, value ) => PgSchema.Unversioned.Table.Metadata.insert(mdkey, value)(conn) )
  override def updateMetadataKeys( conn : Connection, pairs : (MetadataKey,String)* ) : Unit           = pairs.foreach( ( mdkey, value ) => PgSchema.Unversioned.Table.Metadata.update(mdkey, value)(conn) )

  override def hasMetadataTable( conn : Connection ) : Boolean = Using.resource( conn.getMetaData().getTables(null,null,PgSchema.Unversioned.Table.Metadata.Name,null) )( _.next() )

  override def fetchDumpDir(conn: java.sql.Connection): zio.Task[Option[os.Path]] = ZIO.attemptBlocking:
    Some( os.Path( externalConfig.get( ExternalConfig.Key.`protopost.database-dump-dir` ).getOrElse( throw new MissingConfig( s"Cannot find database dump dir; '${ExternalConfig.Key.`protopost.database-dump-dir`}' not configured." ) ) ) )

  override def runDump( ds : DataSource, mbDbName : Option[String], dumpFile : os.Path ) : Task[Unit] = simpleLocalRunDump( ds, mbDbName, dumpFile )

  val TargetSchemaVersion = LatestSchema.Version

  override def upMigrate(conn : Connection, from : Option[Int]): zio.Task[Unit] =
    def upMigrateFrom_New() : Task[Unit] =
      ZIO.attemptBlocking:
        TRACE.log( "upMigrateFrom_New()" )
        TRACE.log( "Creating metadata table..." )
        PgSchema.Unversioned.Table.Metadata.create( conn )
        insertMetadataKeys(
          conn,
          (MetadataKey.SchemaVersion, "0"),
          (MetadataKey.CreatorAppVersion, BuildInfo.version)
        )
        TRACE.log( "Created metadata table, set version to 0." )
    def upMigrateFrom_0() : Task[Unit] =
      ZIO.attemptBlocking:
        TRACE.log( "upMigrateFrom_0()" )
        Using.resource( conn.createStatement() ): stmt =>
          PgSchema.V1.Type.ProtocolType.create( stmt )
          PgSchema.V1.Type.FeedKind.create( stmt )
          PgSchema.V1.Type.FeedCurationType.create( stmt )
          PgSchema.V1.Table.SeismicNode.create( stmt )
          PgSchema.V1.Table.Destination.create( stmt )
          PgSchema.V1.Table.Poster.create( stmt )
          PgSchema.V1.Table.DestinationPoster.create( stmt )
          PgSchema.V1.Table.Post.create( stmt )
          PgSchema.V1.Table.FeedContainer.create( stmt )
          PgSchema.V1.Table.PostFeedContainer.create( stmt )
          PgSchema.V1.Table.PostFeedGuid.create( stmt )
          PgSchema.V1.Table.PostAuthor.create( stmt )
          PgSchema.V1.Table.PostRevision.create( stmt )
          PgSchema.V1.Table.PostPublicationHistory.create( stmt )
          PgSchema.V1.Table.PostDeleteHistory.create( stmt )
          PgSchema.V1.Table.PostUndeleteHistory.create( stmt )
          PgSchema.V1.Table.PostMedia.create( stmt )
          PgSchema.V1.Sequence.SeismicNodeId.create( stmt )
          PgSchema.V1.Sequence.PosterId.create( stmt )
          PgSchema.V1.Sequence.PostId.create( stmt )
          PgSchema.V1.Index.PostFeedGuidIndex.create( stmt )
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
