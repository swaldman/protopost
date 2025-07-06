package protopost.auth

import protopost.ProtopostException

final class UnsupportedHashedPassword( message : String, cause : Throwable = null ) extends ProtopostException( message, cause )

