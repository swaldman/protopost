package protopost

import scala.annotation.targetName
import com.mchange.mailutil.Smtp

object EmailAddress:
  private[protopost] def apply( s : String ) : EmailAddress = // don't inline with check
    val address = Smtp.Address.parseSingle( s, strict = true )
    require( address.displayName == None, "Only simple e-mail addresses, e.g. 'user@subhost.host.tld', without display names, are supported, not " + address.rendered )
    s
  private[protopost] def s( email : EmailAddress ) : String = email  
opaque type EmailAddress = String

