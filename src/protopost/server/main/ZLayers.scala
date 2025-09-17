package protopost.server.main

import java.util.Properties

import zio.*

import protopost.server.{AppResources,ConfigProperties,ExternalConfig}
import protopost.server.exception.{InsecureConfigurationPropertiesFile,MissingConfig}

import com.mchange.conveniences.javautil.*
import com.mchange.v2.c3p0.*
import javax.sql.DataSource

import com.mchange.milldaemon.util.PidFileManager

import protopost.server.db.PgSchemaManager

object ZLayers:

  private val AcceptableConfigPropertiesPerms =
    val acceptablePermStrings = Set("r--------","rw-------")
    acceptablePermStrings.map( os.PermSet.fromString )

  private val DefaultConfigPropertiesFileName    : String = "protopost.properties"

  private val DefaultConfigPropertiesSearchPaths : List[os.Path] =
    ((os.pwd / DefaultConfigPropertiesFileName).toString :: s"/etc/protopost/${DefaultConfigPropertiesFileName}" :: s"/usr/local/etc/protopost/${DefaultConfigPropertiesFileName}" :: Nil).map( os.Path.apply )

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

  def configProperties( configPropertiesFilePath : Option[os.Path] ) : ZLayer[Any, Throwable, ConfigProperties] =
    ZLayer.fromZIO:
      ZIO.attempt(loadConfigProperties(configPropertiesFilePath))

  val shutdownHooks : ZLayer[Any, Throwable, ShutdownHooks] =
    ZLayer.fromZIO:
      ZIO.attempt( PidFileManager.installShutdownHookCarefulDelete() ) *> ZIO.succeed( new ShutdownHooks() )

  val appResources : ZLayer[ConfigProperties, Throwable, AppResources] = ZLayer.fromFunction( (cp : ConfigProperties) => new AppResources(cp) )






