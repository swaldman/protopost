package protopost.jwt

import scala.annotation.targetName

object Jwt:
  private[protopost] inline def apply( s : String ) : Jwt = s
  private[protopost] inline def s(jwt : Jwt) : String = jwt
opaque type Jwt = String


