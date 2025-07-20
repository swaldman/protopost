package protopost.main

import com.monovore.decline.*
import cats.implicits.* // for mapN
import java.nio.file.{Path as JPath}

import protopost.EmailAddress

import com.mchange.rehash.Password

object Decline:
  object Subcommand:
    val createUser =
      val header = "Create a new user."
      val opts =
        val email =
          val help = "The e-mail of the new user."
          Opts.option[String]("email",help=help,metavar="address",short="e").map( EmailAddress.apply )
        val password =
          val help = "The initial password of the new user."
          Opts.option[String]("password",help=help,metavar="password",short="p").map( Password.apply )
        val fullName =
          val help = "The (usually quoted!) full name of the new user."
          Opts.option[String]("full-name",help=help,metavar="name",short="n")
        ( email, password, fullName ) mapN: (e, p, fn) =>
          ConfiguredCommand.CreateUser( e, p, fn )
      Command("create-user", header=header )( opts )
    val daemon =
      val header = "Run a daemon process that serves the protopost API."
      val opts =
        val help = "The port on which the daemon process should serve the API (perhaps only internally, if the API is proxied)."
        Opts.option[Int]("port",help=help).orNone.map( port => ConfiguredCommand.Daemon(port) )
      Command("daemon", header=header )( opts )
    val dbDump =
      val header = "Dump a backup of the database into a configured directory."
      val opts = Opts( ConfiguredCommand.DbDump )
      Command("db-dump", header=header )( opts )
    val dbInit =
      val header = "Initialize the database schema."
      val opts = Opts( ConfiguredCommand.DbInit )
      Command("db-init", header=header )( opts )
    val dbMigrate =
      val header = "Migrate to the latest version of the database schema."
      val opts =
        val help = "Force migration even if the application can find no recent database dump."
        Opts.flag("force",help=help,short="f").orFalse.map( force => ConfiguredCommand.DbMigrate(force) )
      Command("db-migrate", header=header )( opts )
    val generatePrivateKey =
      val header = "Generate and print to the console a hex value suitable for use in config as 'protopost.server.private-key-hex'"
      val opts = Opts( Precommand.GeneratePrivateKey )
      Command("generate-private-key", header=header )( opts )
    val version =
      val header = "Print the version of protopost, then exit."
      val opts = Opts( Precommand.Version )
      Command("version", header=header )( opts )
  end Subcommand

  val protopost =
    val opts =
      val config =
        val help = "Path to properties file containing configuration details."
        val opt  = Opts.option[JPath]("config",help=help,metavar="propsfile").map( os.Path.apply )
        val env  = Opts.env[JPath]("PROTOPOST_CONFIG", help=help).map( os.Path.apply )
        (opt orElse env).orNone
      val subcommands = Opts.subcommands(
        Subcommand.createUser,
        Subcommand.daemon,
        Subcommand.dbDump,
        Subcommand.dbInit,
        Subcommand.dbMigrate,
        Subcommand.generatePrivateKey,
        Subcommand.version
      )
      ( config, subcommands ).mapN( (c,sc) => Tuple2( c, sc ) )
    Command(name="protopost", header="Accept and manage web-based posts to Seismic-managed static-site generators.")( opts )  
end Decline // please.

