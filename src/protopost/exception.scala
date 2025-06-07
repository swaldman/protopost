package protopost

class ProtopostException( message : String, cause : Throwable = null ) extends Exception( message, cause )

final class MissingConfig( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
