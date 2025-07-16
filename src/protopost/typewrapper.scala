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

object PosterId:
  private[protopost] inline def apply( i : Int ) : PosterId = i
opaque type PosterId = Int

extension( pid : PosterId )
  @targetName("posterIdToInt") private[protopost] inline def int : Int = pid
