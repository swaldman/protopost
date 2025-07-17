package protopost.crypto

import org.scalacheck.{Arbitrary,Gen,Properties}
import org.scalacheck.Prop.*
import java.security.interfaces.{ECPrivateKey,ECPublicKey}

object PropertiesKeysAndSignatures extends Properties("Keys And Signatures"):

  given arbitraryKeyPair : Arbitrary[(ECPrivateKey, ECPublicKey)] = Arbitrary( Gen.const[Unit]( () ).map( _ =>  BouncyCastleSecp256r1.generateKeyPair() ) )

  given arbitraryPrivateKeyAndTwoMessages : Arbitrary[( ECPrivateKey, Array[Byte], Array[Byte] )] = {
    val msgGen =
      for
        len <- Gen.choose(0,300)
        bytes <- Gen.containerOfN[Array,Byte]( len, Gen.choose( Byte.MinValue, Byte.MaxValue ).map( _.toByte ) )
      yield bytes
    val gen =
      for
        msg0 <- msgGen
        msg1 <- msgGen
      yield
        ( BouncyCastleSecp256r1.generateKeyPair()(0), msg0, msg1 )
    Arbitrary( gen )
  }

  property("regenerated keys match generated keys") = forAll: (keyPair : (java.security.interfaces.ECPrivateKey, java.security.interfaces.ECPublicKey)) =>
    // println( keyPair )
    BouncyCastleSecp256r1.publicKeyFromS( keyPair(0).getS() ) == keyPair(1)

  property("message signatures verify") = forAll: (pvtKeyMessage : (ECPrivateKey, Array[Byte], Array[Byte])) =>
    val signature = BouncyCastleSecp256r1.sign( pvtKeyMessage(1), pvtKeyMessage(0) )
    val publicKey = BouncyCastleSecp256r1.publicKeyFromPrivate( pvtKeyMessage(0) )
    BouncyCastleSecp256r1.verify( pvtKeyMessage(1), signature, publicKey )

  property("other message, signatures do not verify") = forAll: (pvtKeyMessage : (ECPrivateKey, Array[Byte], Array[Byte])) =>
    val signature = BouncyCastleSecp256r1.sign( pvtKeyMessage(1), pvtKeyMessage(0) )
    val publicKey = BouncyCastleSecp256r1.publicKeyFromPrivate( pvtKeyMessage(0) )
    !BouncyCastleSecp256r1.verify( pvtKeyMessage(2), signature, publicKey )

  property("public keys convert to uncompressed bytes and back") = forAll: (keyPair : (java.security.interfaces.ECPrivateKey, java.security.interfaces.ECPublicKey)) =>
    import BouncyCastleSecp256r1.*
    publicKeyFromUncompressedFormatBytes( publicKeyToUncompressedFormatBytes( keyPair(1) ) ) == keyPair(1)




