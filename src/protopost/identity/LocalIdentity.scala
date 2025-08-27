package protopost.identity

import protopost.crypto.BouncyCastleSecp256r1

import java.security.{PrivateKey,PublicKey}
import java.security.interfaces.{ECPrivateKey,ECPublicKey}

import scala.collection.immutable

object LocalIdentity:
  case class ES256(val location : Location.Simple, val service : Service, val privateKey : ECPrivateKey, val publicKey : ECPublicKey) extends LocalIdentity[ECPrivateKey,ECPublicKey]:
    val algcrv : String = "ES256(P-256)"
    lazy val publicKeyBytes : immutable.ArraySeq[Byte] = immutable.ArraySeq.ofByte(BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes( publicKey ))
    lazy val toPublicIdentity : PublicIdentity.ES256 = PublicIdentity.ES256(location, service, publicKey)
trait LocalIdentity[KPVT <: PrivateKey, KPUB <: PublicKey]:
  def location   : Location.Simple
  def service    : Service
  def algcrv     : String
  def privateKey : KPVT
  def publicKey  : KPUB
  def publicKeyBytes : immutable.ArraySeq[Byte]
  def toPublicIdentity : PublicIdentity[KPUB]


