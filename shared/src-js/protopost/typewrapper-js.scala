package protopost

import scala.scalajs.js.RegExp

object EmailAddress:

  private[protopost] def isValidEmail(email: String): Boolean = {
    val emailRegex = new RegExp("""^[^\s@]+@[^\s@]+\.[^\s@]+$""")
    emailRegex.test(email)
  }
  private[protopost] def apply( s : String ) : EmailAddress = // don't inline with check
    require(isValidEmail(s), s"'${s}' is not a valid e-mail address." )
    // more validations TK, maybe?
    s
  private[protopost] inline def s( email : EmailAddress ) : String = email
opaque type EmailAddress = String


