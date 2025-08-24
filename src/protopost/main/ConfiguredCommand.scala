package protopost.main

import java.sql.Connection
import javax.sql.DataSource

import scala.util.control.NonFatal

import zio.*
import zio.http.Server as ZServer

import sttp.tapir.server.interceptor.log.DefaultServerLog
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}

import protopost.{AppResources,EmailAddress,ExternalConfig,ProtopostException}
import protopost.LoggingApi.*
import protopost.api.TapirEndpoint
import protopost.db.PgSchemaManager

import com.mchange.rehash.*

import com.mchange.sc.zsqlutil.*
import com.mchange.sc.sqlutil.migrate.DbVersionStatus

import com.mchange.milldaemon.util.PidFileManager

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
  case class Daemon( fork : Boolean, verbose : Boolean, port : Option[Int] ) extends ConfiguredCommand:
    val VerboseServerInterpreterOptions: ZioHttpServerOptions[Any] =
    // modified from https://github.com/longliveenduro/zio-geolocation-tapir-tapir-starter/blob/b79c88b9b1c44a60d7c547d04ca22f12f420d21d/src/main/scala/com/tsystems/toil/Main.scala
      ZioHttpServerOptions
        .customiseInterceptors
        .serverLog(
          DefaultServerLog[Task](
            doLogWhenReceived = msg => INFO.zlog(msg),
            doLogWhenHandled = (msg, error) => error.fold(INFO.zlog(msg))(err => WARNING.zlog(s"msg: ${msg}, err: ${err}")),
            doLogAllDecodeFailures = (msg, error) => error.fold(INFO.zlog(msg))(err => WARNING.zlog(s"msg: ${msg}, err: ${err}")),
            doLogExceptions = (msg: String, exc: Throwable) => WARNING.zlog(msg, exc),
            noLog = ZIO.unit
          )
        )
        .options
    val DefaltServerInterpreterOptions: ZioHttpServerOptions[Any] = ZioHttpServerOptions.default.widen[Any]
    def interpreterOptions( verbose : Boolean ) = if verbose then VerboseServerInterpreterOptions else DefaltServerInterpreterOptions
    val attemptInstallPidFileDeleteShutdownHook =
      ZIO.attempt( PidFileManager.installShutdownHookCarefulDelete() ).catchSome: t =>
        t match
          case NonFatal(t) => ZIO.succeed(WARNING.log("Throwable while setting up autoremove PID file shutdown hook.", t))
    override def zcommand =
      for
        _        <- if fork then attemptInstallPidFileDeleteShutdownHook else ZIO.unit 
        ar       <- ZIO.service[AppResources]
        ec       =  ar.externalConfig
        p        =  port.getOrElse( ec( ExternalConfig.Key.`protopost.api.local.port` ).toInt )
        seps     =  TapirEndpoint.serverEndpoints(ar)
        httpApp  =  ZioHttpInterpreter(interpreterOptions(verbose)).toHttp(seps)
        _        <- INFO.zlog( s"Serving protopost API on port $p, location with identity '${ar.localIdentity.toPublicIdentity.toIdentifierWithLocation}'" )
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

