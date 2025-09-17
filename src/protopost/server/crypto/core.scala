package protopost.server.crypto

import protopost.server.exception.UnknownAlgorithmOrCurve

import scala.collection.immutable

import java.security.PublicKey
import java.security.interfaces.ECPublicKey

def publicKeyBytesForPublicKey( algcrv : String, publicKey : PublicKey ) : immutable.ArraySeq[Byte] =
  algcrv.toUpperCase match
    case protopost.server.identity.PublicIdentity.ES256.algcrv =>
      val bytes = BouncyCastleSecp256r1.publicKeyToUncompressedFormatBytes(publicKey.asInstanceOf[ECPublicKey])
      immutable.ArraySeq.ofByte(bytes)
    case _ =>
      throw new UnknownAlgorithmOrCurve( s"We do not know how to interpret public keys for [$algcrv].")

