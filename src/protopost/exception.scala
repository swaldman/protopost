package protopost

class ProtopostException( message : String, cause : Throwable = null ) extends Exception( message, cause )

final class BadProtopostUrl( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class MissingConfig( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class InsecureConfigurationPropertiesFile( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class EmailIsAlreadyRegistered( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class SignatureDoesNotVerify( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )

