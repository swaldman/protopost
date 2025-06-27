package protopost.main

import zio.*

import protopost.LoggingApi.*

import com.monovore.decline.Help

object Main extends SelfLogging:
  private final case class Prerequisites( configPropertiesFilePath : Option[os.Path], command : Precommand | ConfiguredCommand )

  private def parsePrerequisites( args : IndexedSeq[String], env : Map[String,String] ) : Either[Help,Prerequisites] =
    Decline.protopost.parse( args, env ).map( (p, cc) => Prerequisites(p, cc) )

  def main( args : Array[String] ) : Unit =
    parsePrerequisites( args.toIndexedSeq, sys.env ) match
      case Left(help) =>
        println(help)
        java.lang.System.exit(1)
      case Right( Prerequisites( configPropertiesFilePath : Option[os.Path], pc : Precommand ) ) =>
        java.lang.System.exit( pc.execute() )
      case Right( Prerequisites( configPropertiesFilePath : Option[os.Path], cc : ConfiguredCommand ) ) =>
        val task =
          cc.zcommand.provide(
            ZLayers.configProperties( configPropertiesFilePath ),
            ZLayers.shutdownHooks,
            ZLayers.externalConfig,
            ZLayers.dataSource,
            ZLayers.pgSchemaManager
          )
        val completionValue =
          Unsafe.unsafely:
            Runtime.default.unsafe.run(task).getOrThrow()
        TRACE.log(s"protopost process ended with completion value: ${completionValue}")
        java.lang.System.exit( completionValue )
end Main
