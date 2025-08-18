package protopost

object Password:
  inline def apply( s : String ) : Password = s
  inline def s( password : Password ) : String = password
opaque type Password = String



