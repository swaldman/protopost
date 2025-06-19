package protopost.main

import java.util.Properties

import zio.*

import protopost.{ExternalConfig,InsecureConfigurationPropertiesFile,MissingConfig}

import com.mchange.conveniences.javautil.*

object ZLayers:
  
  val AcceptableConfigPropertiesPerms =
    val acceptablePermStrings = Set("r--------","rw-------")
    acceptablePermStrings.map( os.PermSet.fromString )

  val DefaultConfigPropertiesFileName    : String = "protopost.properties"
  
  val DefaultConfigPropertiesSearchPaths : List[os.Path] =
    ((os.pwd / DefaultConfigPropertiesFileName).toString :: s"/etc/protopost/${DefaultConfigPropertiesFileName}" :: s"/usr/local/etc/protopost/${DefaultConfigPropertiesFileName}" :: Nil).map( os.Path.apply )

  def shutdownHooks() : ZLayer[Any, Throwable, ShutdownHooks] = ???

  private def loadConfigProperties(configPropertiesFilePath : Option[os.Path]) : ConfigProperties =
    val existing =
      configPropertiesFilePath match
        case Some( cpfp ) => 
          if os.exists( cpfp ) then cpfp else throw new MissingConfig( s"Expected configuration properties file at '${cpfp}'. Not found." )
        case None =>
          DefaultConfigPropertiesSearchPaths.find( os.exists ).getOrElse:
            throw new MissingConfig( s"""Configuration file not found at any of the following default locations: ${DefaultConfigPropertiesSearchPaths.mkString(", ")}""" )
    val perms = os.perms(existing)
    if AcceptableConfigPropertiesPerms(perms) then
      ConfigProperties( loadProperties( existing.toNIO ) )
    else
      val ap = AcceptableConfigPropertiesPerms.map(str => s"'${str}'").mkString(", ")
      throw new InsecureConfigurationPropertiesFile( s"Configuraton properties file '${existing}' is insecure. It has permissions '${perms}', must be one of ${ap}" )

  def configProperties(configPropertiesFilePath : Option[os.Path]) : ZLayer[Option[os.Path], Throwable, ConfigProperties] = ZLayer.fromFunction( loadConfigProperties _ )

  def externalConfig(configProperties : ConfigProperties) : ZLayer[ConfigProperties, Throwable, ExternalConfig] = ZLayer.fromFunction( (cp : ConfigProperties) => ExternalConfig.fromProperties(cp.props) )


