package protopost

object Password:
  private[protopost] inline def apply( s : String ) : Password = s
  private[protopost] inline def s( password : Password ) : String = password
opaque type Password = String

object PosterId:
  private[protopost] inline def apply( i : Int ) : PosterId = i
  private[protopost] inline def i(pid : PosterId) : Int = pid
opaque type PosterId = Int


