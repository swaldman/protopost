package protopost.common

enum Service:
  case protopost, seismic;

enum Protocol( val defaultPort : Int ):
  case http  extends Protocol(80)  // for testing only! auth credentials are sent "in the clear", so only https should be used in production
  case https extends Protocol(443)

object Password:
  private[protopost] inline def apply( s : String ) : Password = s
  private[protopost] inline def s( password : Password ) : String = password
opaque type Password = String

object PosterId:
  private[protopost] inline def apply( i : Int ) : PosterId = i
  private[protopost] inline def i(pid : PosterId) : Int = pid
opaque type PosterId = Int
