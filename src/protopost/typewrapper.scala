package protopost

import java.util.Properties

import scala.annotation.targetName

import com.mchange.mailutil.Smtp

object ConfigProperties:
  private[protopost] def apply( p : Properties ) : ConfigProperties = p
opaque type ConfigProperties = Properties

extension( configProperties : ConfigProperties )
  @targetName("configPropertiesProps") private[protopost] inline def props : Properties = configProperties

object EmailAddress:
  private[protopost] def apply( s : String ) : EmailAddress =
    val address = Smtp.Address.parseSingle( s, strict = true )
    require( address.displayName == None, "Only simple e-mail addresses, e.g. 'user@subhost.host.tld', without display names, are supported, not " + address.rendered )
    s
opaque type EmailAddress = String

extension( email : EmailAddress )
  @targetName("emailAddressToString") private[protopost] inline def str : String = email


object Password:
  private[protopost] inline def apply( s : String ) : Password = s
opaque type Password = String

extension( password : Password )
  @targetName("passwordToString") private[protopost] inline def str : String = password

object BCryptHash:
  private[protopost] def apply( chars : Array[Char] ) : BCryptHash =
    require( chars.length == 60, "A BCrypt salted hash must contain precisely 60 characters, provided hash contains " + chars.length )
    chars
opaque type BCryptHash = Array[Char]

extension( bchash : BCryptHash )
  @targetName("bcryptHashUnsafeInternalArray") private[protopost] inline def unsafeInternalArray : Array[Char] = bchash



