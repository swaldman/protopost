package protopost.crypto

import scala.annotation.targetName

object SignatureSHA256withECDSA:
  def apply( bytes : Array[Byte] ) : SignatureSHA256withECDSA = bytes
opaque type SignatureSHA256withECDSA = Array[Byte]

extension( signatureSHA256withECDSA : SignatureSHA256withECDSA )
  @targetName("signatureSHA256withECDSA") inline def bytes : Array[Byte] = signatureSHA256withECDSA

