package protopost.main

import java.sql.Connection
import javax.sql.DataSource

import scala.collection.immutable
import scala.util.control.NonFatal

import zio.*
import zio.http.Server as ZServer

import sttp.tapir.server.interceptor.log.DefaultServerLog
import sttp.tapir.server.ziohttp.{ZioHttpInterpreter, ZioHttpServerOptions}

import protopost.{AppResources,BadService,EmailAddress,ExternalConfig,InconsistentSeismicNodeDefinition,ProtopostException,SeismicNodeWithId}
import protopost.LoggingApi.*
import protopost.api.{Destination,TapirEndpoint}
import protopost.db.PgSchemaManager
import protopost.identity.{PublicIdentity,Service}

import com.mchange.cryptoutil.given

import com.mchange.rehash.*

import com.mchange.sc.zsqlutil.*
import com.mchange.sc.sqlutil.migrate.DbVersionStatus

import com.mchange.milldaemon.util.PidFileManager

object ConfiguredCommand extends SelfLogging:
  case class CreateDestination( seismicIdentifierWithLocation : String, destinationName : String, force : Boolean ) extends ConfiguredCommand:
    val publicIdentity = PublicIdentity.fromIdentifierWithLocationSimple(seismicIdentifierWithLocation)
    def matches( seismicNodeWithId : SeismicNodeWithId ) : Boolean =
      publicIdentity.service == Service.seismic                 &&
      publicIdentity.algcrv == seismicNodeWithId.algcrv         &&
      publicIdentity.publicKeyBytes == seismicNodeWithId.pubkey &&
      publicIdentity.location == seismicNodeWithId.location
    def ensureSeismic : Task[Unit] =
      if publicIdentity.service == Service.seismic then ZIO.unit
      else throw new BadService( s"Destinations much live on seismic, not ${publicIdentity.service} nodes, as in identifier provided '${seismicIdentifierWithLocation}'." )
    def findCreateSeismicNode( ar : AppResources, conn : Connection ) : Task[Int] =  
      val checkSeismicNodesByHostPort : Task[Option[Int]] = // we return an int, its id, if this seismic node is already known
        for
          sns  <- ZIO.attemptBlocking( ar.database.seismicNodesByHostPort( conn, publicIdentity.location.host, publicIdentity.location.port ) )
        yield
          if sns.isEmpty then
            None
          else
            sns.find( matches ) match
              case Some( sn ) => Some( sn.id )
              case None =>
                val msg =
                  s"Distinct seismic nodes are already defined on host '${publicIdentity.location.host}' and port '${publicIdentity.location.port}': " +
                  sns.map( _.identifierWithLocation ).mkString("[",",","]")
                if !force then
                  throw new InconsistentSeismicNodeDefinition( s"${msg} Consider correcting either the current destination, or updating the existing seismic nodes for consistency." )
                else
                  WARNING.log( msg + " Since '--force' was specified, we will define a new node at this location anyway! You may wish to verify these nodes!" )
                  None
      val checkSeismicNodesByPublicKey : Task[Option[Int]] = // we return an int, its id, if this seismic node is already known
        for
          sns  <- ZIO.attemptBlocking( ar.database.seismicNodesByPubkey( conn, publicIdentity.publicKeyBytes.unsafeArray.asInstanceOf[Array[Byte]] ) )
        yield
          if sns.isEmpty then
            None
          else
            sns.find( matches ) match
              case Some( sn ) => Some( sn.id )
              case None =>
                val msg =
                  s"Distinct seismic nodes are already defined with the same public key '${publicIdentity.publicKeyBytes.hex0x}': " +
                  sns.map( _.identifierWithLocation ).mkString("[",",","]")
                if force then
                  throw new InconsistentSeismicNodeDefinition( s"${msg} Consider correcting either the current destination, or updating the existing seismic nodes for consistency." )
                else
                  WARNING.log( msg + " Since '--force' was specified, we will define a new node at this location anyway! You may wish to verify these nodes!" )
                  None
      val checkForExistingSeismicNodes : Task[Option[Int]] =
        for
          mbFromHostPort <- checkSeismicNodesByHostPort
          mbFromPubKey   <- if mbFromHostPort.isEmpty then checkSeismicNodesByHostPort else ZIO.none // don't check again if we've already found our node
        yield
          mbFromHostPort orElse mbFromPubKey
      checkForExistingSeismicNodes.map: mbId =>
        mbId.getOrElse( ar.database.newSeismicNode( conn, publicIdentity.algcrv, publicIdentity.publicKeyBytes.unsafeArray.asInstanceOf[Array[Byte]], publicIdentity.location.protocol, publicIdentity.location.host, publicIdentity.location.port ) )
    def createDestination( ar : AppResources, conn : Connection ) : Task[Destination]=
      for
        seismicNodeId <- findCreateSeismicNode( ar, conn )
      yield
        ar.database.newDestination( conn, seismicNodeId, destinationName )
    override def zcommand =
      for
        _    <- ensureSeismic
        ar   <- ZIO.service[AppResources]
        ds   = ar.dataSource
        d   <- withConnectionTransactionalZIO( ds )( conn => createDestination( ar, conn ) )
      yield
        INFO.log(s"Created new destination with name '${d.name}' on seismic node '${d.seismicIdentifierWithLocation}'")
        0
  end CreateDestination
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
  case object ShowIdentifier extends ConfiguredCommand:
    override def zcommand =
      for
        ar <- ZIO.service[AppResources]
      yield
        println( ar.localIdentity.toPublicIdentity.toIdentifierWithLocation )
        0
    end zcommand
  case object ListDestinations extends ConfiguredCommand:
    import com.mchange.sc.v1.texttable.*
    val Columns = Seq( Column("Seismic Node Identifier With Location"), Column("Name") )
    given Ordering[Destination] = Ordering.by( (d : Destination )=> ( d.seismicIdentifierWithLocation, d.name) )
    override def zcommand =
      for
        ar  <- ZIO.service[AppResources]
        db  =  ar.database
        ds  =  ar.dataSource
        ds <- withConnectionTransactional(ds)( db.allDestinations )
      yield
        val rows = immutable.SortedSet.from(ds).toList.map( Row.apply )
        printProductTable( Columns )( rows )
        0
    end zcommand

sealed trait ConfiguredCommand:
  def zcommand : ZIO[ShutdownHooks & AppResources, Throwable, Int]

