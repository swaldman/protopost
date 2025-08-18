package protopost

import java.util.Properties

import scala.annotation.targetName

object ConfigProperties:
  private[protopost] inline def apply( p : Properties ) : ConfigProperties = p
opaque type ConfigProperties = Properties

extension( configProperties : ConfigProperties )
  @targetName("configPropertiesProps") private[protopost] inline def props : Properties = configProperties

object PosterId:
  private[protopost] inline def apply( i : Int ) : PosterId = i
opaque type PosterId = Int

extension( pid : PosterId )
  @targetName("posterIdToInt") private[protopost] inline def int : Int = pid

