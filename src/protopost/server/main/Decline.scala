package protopost.server.main

import com.monovore.decline.*
import cats.implicits.* // for mapN
import java.nio.file.{Path as JPath}

import protopost.common.{EmailAddress,PosterId}
import protopost.server.ProtoSeismicNode

import com.mchange.rehash.Password
import protopost.server.main.Decline.Common.fullDestination

object Decline:
  object Common:
    val acceptAdvertised =
      val help = "Accept the identity that a remote node advertises via its JWKS."
      Opts.flag("accept-advertised",help=help,short="a").orFalse
    val destinationName =
      val help = "The name of the site on the remote seismic node."
      Opts.option[String]("name",help=help,metavar="site-name")
    val posterIdOrEmailAddress : Opts[PosterId | EmailAddress] =
      val posterId =
        val help = s"The ID of a poster, available via 'list-posters'."
        Opts.option[Int]("user-id", help=help).map( PosterId.apply )
      val email =
        val help = s"The email address of a poster, available via 'list-posters'."
        Opts.option[String]("email", help=help).map( EmailAddress.apply )
      posterId.orElse(email)
    val seismicNode : Opts[ProtoSeismicNode] =
      val help = "The full identifier, including location, of the seismic node hosting the destination."
      Opts.option[String]("seismic-node",help=help,metavar="identifier").map( ProtoSeismicNode.apply )
    val fullDestination : Opts[(ProtoSeismicNode,String,Boolean)] = // we have to specify this late, since it's built on two earlier vals
      ( seismicNode, destinationName, acceptAdvertised ).mapN( ( sn, dn, aa ) => Tuple3(sn,dn,aa) )
  object Subcommand:
    val createDestination =
      val header = "Create a new destination, a reference to a site on a seismic node to which posts can be published."
      val opts =
        ( Common.seismicNode, Common.destinationName, Common.acceptAdvertised ) mapN: (sn, n, aa) =>
          ConfiguredCommand.CreateDestination( sn, n, aa )
      Command("create-destination", header=header )( opts )
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
        val fork =
          val help = "Run as background process (if supported by wrapper script) and generate a PID file."
          Opts.flag("fork",help=help).orFalse
        val verbose =
          val help = "Log requests, verbosely."
          Opts.flag("verbose",help=help).orFalse
        val port =
          val help = "The port on which the daemon process should serve the API (perhaps only internally, if the API is proxied)."
          Opts.option[Int]("port",help=help).orNone
        ( fork, verbose, port ) mapN: (f, v, p) =>
          ConfiguredCommand.Daemon(f,v,p)
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
    val grantDestination =
      val header = "Grant a user the right to post a destination."
      val opts =
        val nickname =
          val help = s"A nickname for the destination visible to the poster."
          Opts.option[String]("nickname", help=help).orNone 
        ( Common.seismicNode, Common.destinationName, Common.posterIdOrEmailAddress, nickname, Common.acceptAdvertised ) mapN: (sn, n, poea, nn, aa) =>
          ConfiguredCommand.GrantDestination( sn, n, poea, nn, aa )
      Command("grant-destination", header=header )( opts )
    val generatePrivateKey =
      val header = "Generate and print to the console a hex value suitable for use in config as 'protopost.server.private-key-hex'"
      val opts = Opts( Precommand.GeneratePrivateKey )
      Command("generate-private-key", header=header )( opts )
    val listDestinations =
      val header = "List the destinations known to this server."
      val opts =
        val mbPoster =
          Common.posterIdOrEmailAddress.orNone
        mbPoster.map( ConfiguredCommand.ListDestinations.apply )
      Command("list-destinations", header=header )( opts )
    val listUsers =
      val header = "List the users known to this server."
      val opts = fullDestination.orNone.map( ConfiguredCommand.ListUsers.apply )
      Command("list-users", header=header )( opts )
    val showIdentifier =
      val header = "Print to the console the full identifier-with-location of this protopost node, suitable for identifying it to seismic nodes."
      val opts = Opts( ConfiguredCommand.ShowIdentifier )
      Command("show-identifier", header=header )( opts )
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
        Subcommand.createDestination,
        Subcommand.createUser,
        Subcommand.daemon,
        Subcommand.dbDump,
        Subcommand.dbInit,
        Subcommand.dbMigrate,
        Subcommand.grantDestination,
        Subcommand.generatePrivateKey,
        Subcommand.listDestinations,
        Subcommand.listUsers,
        Subcommand.showIdentifier,
        Subcommand.version
      )
      ( config, subcommands ).mapN( (c,sc) => Tuple2( c, sc ) )
    Command(name="protopost", header="Accept and manage web-based posts to Seismic-managed static-site generators.")( opts )  
end Decline // please.

