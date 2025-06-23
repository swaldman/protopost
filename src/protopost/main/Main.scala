package protopost.main

import protopost.LoggingApi.*

object Main extends SelfLogging:
  private final case class Prerequisites( configPropertiesFilePath : Option[os.Path], cc : ConfiguredCommand )

  private def parsePrerequisites( args : IndexedSeq[String], env : Map[String,String] ) : Either[String,Prerequisites] = ???

  def main( args : Array[String] ) : Unit =
    parsePrerequisites( args.toIndexedSeq, sys.env ) match
      case Left(help) =>
        println(help)
        System.exit(1)
      case Right( Prerequisites( configPropertiesFilePath : Option[os.Path], cc : ConfiguredCommand ) ) =>
        import zio.*
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
        TRACE.log(s"Feedletter process ended with completion value: ${completionValue}")
end Main
