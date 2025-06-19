package protopost.main

import java.util.Properties

import scala.annotation.targetName

object ConfigProperties:
  private[main] def apply( p : Properties ) : ConfigProperties = p
opaque type ConfigProperties = Properties

extension( configProperties : ConfigProperties )
  @targetName("configPropertiesProps") inline def props : Properties = configProperties

