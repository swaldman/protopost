package protopost

import scala.annotation.targetName

object EmailAddress:
  private[protopost] def apply( s : String ) : EmailAddress = // don't inline with check
    require( s.indexOf('@') >= 0, s"An e-mail address must contain an '@' sign, $s does not" )
    // more validations TK, maybe?
    s
  private inline def s( email : EmailAddress ) : String = email
opaque type EmailAddress = String


