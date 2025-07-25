package protopost.main

import java.sql.Connection
import javax.sql.DataSource

import zio.*
import zio.http.Server as ZServer

import sttp.tapir.server.ziohttp.ZioHttpInterpreter

import protopost.{AppResources,EmailAddress,ExternalConfig,ProtopostException,Server}
import protopost.LoggingApi.*
import protopost.api.TapirEndpoint
import protopost.db.PgSchemaManager

import com.mchange.rehash.*

import com.mchange.sc.zsqlutil.*
import com.mchange.sc.sqlutil.migrate.DbVersionStatus

object ConfiguredCommand extends SelfLogging:
  case class CreateUser( email : EmailAddress, password : Password, fullName : String ) extends ConfiguredCommand:
    override def zcommand =
      for
        ar   <- ZIO.service[AppResources]
        ds   = ar.dataSource
        id   <- ar.database.txn.createUser( ds, ar.authManager )( email, fullName, password )
      yield
        INFO.log(s"Created new user '${fullName}' with email '${email}' and id '${id}'")
        0
  end CreateUser
  case class Daemon( port : Option[Int] ) extends ConfiguredCommand:
    override def zcommand =
      for
        ar       <- ZIO.service[AppResources]
        ec       =  ar.externalConfig
        p        =  port.getOrElse( Server.Identity(ec).location.port )
        seps     =  TapirEndpoint.serverEndpoints(ar)
        httpApp  =  ZioHttpInterpreter().toHttp(seps)
        _        <- INFO.zlog( s"Serving protopost API on port $p" )
        exitCode <- ZServer
                      .serve(httpApp)
                      .tapDefect( c => FATAL.zlog("API web server failed unexpectedly, cause: " + c ) )
                      .provide(ZLayer.succeed(ZServer.Config.default.port(p)), ZServer.live)
                      .exitCode
      yield
        exitCode.code
    end zcommand
  case object DbDump extends ConfiguredCommand:
    override def zcommand =
      for
        ar  <- ZIO.service[AppResources]
        out <- ar.schemaManager.dump(ar.dataSource)
      yield
        INFO.log(s"The database was successfully dumped to '${out}'.")
        0
    end zcommand
  case object DbInit extends ConfiguredCommand:
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
    private def dbInitTask( ar : AppResources ) : Task[Int] =
      withConnectionTransactionalZIO( ar.dataSource ): conn =>
        for
          vstatus <- ar.schemaManager.dbVersionStatus(conn)
          code    <- taskForStatus( ar.schemaManager, conn, vstatus )
        yield code
    def zcommand =
      for
        ar      <- ZIO.service[AppResources]
        code    <- dbInitTask( ar )
      yield code
    end zcommand
  case class DbMigrate( force : Boolean ) extends ConfiguredCommand:
    override def zcommand =
      for
        ar <- ZIO.service[AppResources]
        sm =  ar.schemaManager
        ds =  ar.dataSource
        _  <- if force then sm.migrate(ds) else sm.cautiousMigrate(ds)
      yield
        // migrate functions log internally, no need for additional messages here
        0
    end zcommand

sealed trait ConfiguredCommand:
  def zcommand : ZIO[ShutdownHooks & AppResources, Throwable, Int]

