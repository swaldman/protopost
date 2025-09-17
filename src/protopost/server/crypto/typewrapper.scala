package protopost.server.crypto

import scala.annotation.targetName

object SignatureSHA256withECDSA:
  inline def apply( bytes : Array[Byte] ) : SignatureSHA256withECDSA = bytes
opaque type SignatureSHA256withECDSA = Array[Byte]

extension( signatureSHA256withECDSA : SignatureSHA256withECDSA )
  @targetName("signatureSHA256withECDSAunsafeInternalArray") inline def unsafeInternalArray : Array[Byte] = signatureSHA256withECDSA

