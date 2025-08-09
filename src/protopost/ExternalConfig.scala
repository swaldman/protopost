package protopost

import java.util.Properties
import scala.jdk.CollectionConverters.*
import protopost.MissingConfig

object ExternalConfig:
  enum Key:
    case `protopost.database-dump-dir`
    case `protopost.server.private-key-hex`
    case `protopost.server.url`
    case `protopost.token.security.high.validity.minutes`
    case `protopost.token.security.low.validity.minutes`
    case `protopost.api.port`

  val Defaults = Map (
    Key.`protopost.token.security.high.validity.minutes` -> 120.toString,
    Key.`protopost.token.security.low.validity.minutes`  -> (2 * 24 * 60).toString,
    Key.`protopost.api.port`                             -> 8025.toString,
  )

  def fromProperties( props : Properties ) : ExternalConfig = new ExternalConfig:
    val map =
      val builder = Map.newBuilder[ExternalConfig.Key,String]
      val found =
        ExternalConfig.Key.values.foldLeft( Map.empty[ExternalConfig.Key,String] ): (accum, next) =>
          val value = props.getProperty( next.toString )
          if value == null then accum else accum + Tuple2( next, value )
      builder ++= Defaults
      builder ++= found
      builder.result()

    def get( key : ExternalConfig.Key ) : Option[String] = map.get( key )

trait ExternalConfig:
  def get( key : ExternalConfig.Key ) : Option[String]
  def apply( key : ExternalConfig.Key ) : String = get(key).getOrElse( throw new MissingConfig( key.toString() ) )
