package protopost

import java.util.Properties

import scala.annotation.targetName

object ConfigProperties:
  private[protopost] inline def apply( p : Properties ) : ConfigProperties = p
opaque type ConfigProperties = Properties

extension( configProperties : ConfigProperties )
  @targetName("configPropertiesProps") private[protopost] inline def props : Properties = configProperties


