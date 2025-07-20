package protopost.jwt

import scala.annotation.targetName

object Jwt:
  private[protopost] inline def apply( s : String ) : Jwt = s
opaque type Jwt = String

extension( jwt : Jwt )
  @targetName("jwtToString") private[protopost] inline def str : String = jwt


