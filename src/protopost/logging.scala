package protopost

object LoggingApi: // workaround of nonexporting of SelfLogging from logadapter, due to a compiler bug. hopefully unnecessary soon
  val raw = logadapter.zio.ZApi( logadapter.log4j2.Api )
  type SelfLogging = raw.inner.SelfLogging
  export raw.*
