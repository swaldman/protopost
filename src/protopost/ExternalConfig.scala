package protopost

import java.util.Properties

object ExternalConfig:
  enum Key:
    case `protopost.database-dump-dir`
    case `protopost.server.private-key-hex`
    case `protopost.server.url`

  def fromProperties( props : Properties ) : ExternalConfig = new ExternalConfig:
    def get( key : ExternalConfig.Key ) : Option[String] = Option( props.getProperty( key.toString ) )

trait ExternalConfig:
  def get( key : ExternalConfig.Key ) : Option[String]
