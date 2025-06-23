package protopost.main

import com.monovore.decline.*
import cats.implicits.* // for mapN
import java.nio.file.{Path as JPath}

object Decline:
  object Subcommand:
    val dbInit =
      val header = "Initialize the database schema."
      val opts = Opts( ConfiguredCommand.DbInit )
      Command("db-init", header=header )( opts )
  end Subcommand  

  val protopost =
    val opts =
      val config =
        val help = "Path to properties file containing configuration details."
        val opt  = Opts.option[JPath]("config",help=help,metavar="propsfile").map( os.Path.apply )
        val env  = Opts.env[JPath]("PROTOPOST_CONFIG", help=help).map( os.Path.apply )
        (opt orElse env).orNone
      val subcommands = Opts.subcommands(
        Subcommand.dbInit
      )
      ( config, subcommands ).mapN( (c,sc) => Tuple2( c, sc ) )
    Command(name="protopost", header="Accept and manage web-based posts to Seismic-managed static-site generators.")( opts )  
end Decline // please.

