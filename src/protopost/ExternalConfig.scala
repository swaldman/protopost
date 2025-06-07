package protopost

object ExternalConfig:
  enum Key:
    case DatabaseDumpDir extends Key
trait ExternalConfig:
  def get( key : ExternalConfig.Key ) : Option[String]

