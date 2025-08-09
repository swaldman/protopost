package protopost

class ProtopostException( message : String, cause : Throwable = null ) extends Exception( message, cause )

final class BadServiceUrl( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class UnsupportedProtocol( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class MissingConfig( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class InsecureConfigurationPropertiesFile( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class EmailIsAlreadyRegistered( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class SignatureDoesNotVerify( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class BadCredentials( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class PosterUnknown( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class UnknownAlgorithmOrCurve( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class BadLocation( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
final class BadIdentifierFormat( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )
