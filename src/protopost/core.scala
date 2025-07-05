package protopost

import protopost.crypto.BouncyCastleSecp256r1

import java.security.interfaces.ECPrivateKey
import java.security.interfaces.ECPublicKey

object Server:
  object Location:
    val DefaultPort = 80
    val Default = Location("localhost", DefaultPort)
    val UrlRegex = """^protopost\:\/\/(localhost|(?:(?:[a-zA-Z_0-9]\.)+[a-zA-Z_0-9]+))(?:\:(\d+))?(?:\/)?$""".r
    def apply( putativeProtopostUrl : String ) : Location =
      putativeProtopostUrl match
        case UrlRegex( host, port ) => Location( host, if port.isEmpty then DefaultPort else port.toInt )
        case _ => throw new BadProtopostUrl( s"'${putativeProtopostUrl}' is not a valid protopost URL, which should look like 'protopost://<host>/' or 'protopost://<host>:<port>/'" )
  case class Location( host : String, port : Int ):
    def toUrl : String =
      if port == Location.DefaultPort then s"protopost://${host}/" else s"protopost://${host}:${port}/"

  object Identity:
    def apply( ecfg : ExternalConfig ) : Identity =
      val pvtKeyHexKey = ExternalConfig.Key.`protopost.server.private-key-hex`
      val hex = ecfg.get(pvtKeyHexKey).getOrElse( throw new MissingConfig(s"Cannot establish server identity with config key '$pvtKeyHexKey' unset.") )
      val privateKey = BouncyCastleSecp256r1.privateKeyFromHex( hex )
      val publicKey = BouncyCastleSecp256r1.publicKeyFromPrivate( privateKey )
      val location = 
        ecfg.get(ExternalConfig.Key.`protopost.server.url`) match
          case Some(url) => Location(url)
          case None      => Location.Default
      Identity( privateKey, publicKey, location )
  case class Identity( privateKey : ECPrivateKey, publicKey : ECPublicKey, location : Server.Location )
