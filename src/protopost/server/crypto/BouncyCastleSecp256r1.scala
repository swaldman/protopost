package protopost.server.crypto

import java.math.BigInteger

import java.security.interfaces.ECPublicKey
import java.security.interfaces.ECPrivateKey
import java.security.spec.{ECParameterSpec,ECGenParameterSpec,ECPublicKeySpec,ECPrivateKeySpec}
import java.security.{AlgorithmParameters,KeyFactory,KeyPair,KeyPairGenerator,SecureRandom,Security,Signature}
import java.security.spec.ECPoint

import org.bouncycastle.asn1.sec.SECNamedCurves
import org.bouncycastle.crypto.params.ECDomainParameters

import com.mchange.cryptoutil.{*,given}
import java.util.Base64

object BouncyCastleSecp256r1:
  private val Provider            = BouncyCastle.Provider
  private val ProviderName        = BouncyCastle.ProviderName
  private val KeyAlgoName         = "EC"
  private val KeyAlgoNameSpecific = "ECDSA"

  private val ECParamBundleName = "secp256r1"
  private val ECGenParamSpec = new ECGenParameterSpec(ECParamBundleName)
  private val Params = SECNamedCurves.getByName(ECParamBundleName);
  private val Curve = new ECDomainParameters(Params.getCurve(), Params.getG(), Params.getN(), Params.getH());

  private val entropy = new SecureRandom()

  // private val SignatureAlgoName = "SHA256withPLAIN-ECDSA"

  // consistent with https://github.com/auth0/java-jwt/blob/master/lib/src/main/java/com/auth0/jwt/algorithms/Algorithm.java
  // see
  //   https://stackoverflow.com/questions/39385718/der-decode-ecdsa-signature-in-java/78555671#78555671
  //   https://stackoverflow.com/questions/34063694/fixed-length-64-bytes-ec-p-256-signature-with-jce
  // for decoding

  // these signatures do not have a constant length
  private val SignatureAlgoName = "SHA256withECDSA" 


  // modified from consuela
  private val ECParamSpec =
    val algorithmParameters = AlgorithmParameters.getInstance( KeyAlgoName, ProviderName ); // more specific ECDSA does not work here
    algorithmParameters.init( ECGenParamSpec );
    algorithmParameters.getParameterSpec( classOf[ECParameterSpec] )


  // modified from https://stackoverflow.com/questions/42639620/generate-ecpublickey-from-ecprivatekey
  def generateKeyPair() : ( ECPrivateKey, ECPublicKey ) =
    val keyPairGenerator = KeyPairGenerator.getInstance(KeyAlgoNameSpecific, ProviderName)
    keyPairGenerator.initialize(ECGenParamSpec, entropy)
    val pair = keyPairGenerator.generateKeyPair()
    (pair.getPrivate().asInstanceOf[ECPrivateKey], pair.getPublic().asInstanceOf[ECPublicKey])


  // modified from consuela code
  def privateKeyFromS( s : BigInt ) : ECPrivateKey =
    val kf = KeyFactory.getInstance( KeyAlgoNameSpecific, ProviderName ); // XXX: is this KeyFactory immutable or thread-safe? can i cache it?
    val privSpec = new ECPrivateKeySpec( s.bigInteger, ECParamSpec );
    kf.generatePrivate( privSpec ).asInstanceOf[ECPrivateKey];

  def privateKeyFromHex( hex : String ) : ECPrivateKey =
    val bytes = hex.decodeHexToArray
    privateKeyFromS( bytes.toUnsignedBigInt )

  /**
   *  @return a 64 byte / 512 bit byte array which is the concatenation of the byte representations
   *          of two 256 bit big-endian ints X and Y
   */
  def computePublicKeyBytes( privateKeyS : java.math.BigInteger ) : Array[Byte] = // modified from consuela
    val rawKey = Curve.getG().multiply( privateKeyS ).getEncoded( false );
    assert( rawKey(0) == 0x04 && rawKey.length == 65, "Computed public key is not in the expected uncompressed format." )
    rawKey.drop(1)  // drop the header byte 0x04 that signifies an uncompressed concatenation of values

  def publicKeyPointFromS( s : BigInt ) : ECPoint =
    val rawBytes = computePublicKeyBytes( s.bigInteger )
    val xBytes = rawBytes.take(32)
    val yBytes = rawBytes.drop(32)
    ECPoint( xBytes.toUnsignedBigInteger, yBytes.toUnsignedBigInteger )

  def publicKeyFromS( s : BigInt ) : ECPublicKey =
    val spec = ECPublicKeySpec(publicKeyPointFromS(s), ECParamSpec)
    KeyFactory.getInstance(KeyAlgoNameSpecific, ProviderName).generatePublic(spec).asInstanceOf[ECPublicKey]

  def publicKeyFromPrivate( privateKey : ECPrivateKey ) : ECPublicKey = publicKeyFromS( privateKey.getS() )

  // modified from https://tonisives.com/blog/2021/03/31/java-ec-crypto-with-bouncycastle/
  def signToByteArray( message : Array[Byte], privateKey : ECPrivateKey ) : Array[Byte] =
    val signer = Signature.getInstance(SignatureAlgoName, Provider)
    signer.initSign(privateKey)
    signer.update(message)
    signer.sign()

  // modified from https://tonisives.com/blog/2021/03/31/java-ec-crypto-with-bouncycastle/
  def verifySignatureAsByteArray( message : Array[Byte], signature : Array[Byte], publicKey : ECPublicKey ) : Boolean =
    val verifier = Signature.getInstance(SignatureAlgoName, Provider)
    verifier.initVerify(publicKey)
    verifier.update(message)
    verifier.verify(signature)

  def sign[T : Byteable]( message : T, privateKey : ECPrivateKey ) : SignatureSHA256withECDSA = SignatureSHA256withECDSA(signToByteArray( message.toByteArray, privateKey ))
  def verify[T : Byteable]( message : T, signature : SignatureSHA256withECDSA, publicKey : ECPublicKey ) : Boolean = verifySignatureAsByteArray( message.toByteArray, signature.unsafeInternalArray, publicKey )

  def publicKeyToUncompressedFormatBytes( publicKey : ECPublicKey ) : Array[Byte] =
    val w = publicKey.getW()
    val xBytes = w.getAffineX().unsignedBytes(32)
    val yBytes = w.getAffineY().unsignedBytes(32)
    val out = Array.ofDim[Byte](65)
    out(0) = 0x04 // header
    Array.copy(xBytes,0,out,1,32)
    Array.copy(yBytes,0,out,33,32)
    out

  def publicKeyFromUncompressedFormatBytes( bytes : Array[Byte] ) : ECPublicKey =
    require( bytes(0) == 0x04 && bytes.length == 65, "Public key is not in the expected uncompressed format." )
    val ( xBytes, yBytes ) = bytes.drop(1).splitAt(32)
    assert( xBytes.length == 32 && yBytes.length == 32 )
    val point = ECPoint( xBytes.toUnsignedBigInteger, yBytes.toUnsignedBigInteger )
    val spec = ECPublicKeySpec(point, ECParamSpec)
    KeyFactory.getInstance(KeyAlgoNameSpecific, ProviderName).generatePublic(spec).asInstanceOf[ECPublicKey]

  def fieldValueToHex(  bi : BigInt     ) : String = bi.unsignedBytes(32).hex
  def fieldValueToHex( jbi : BigInteger ) : String = fieldValueToHex( jbi.toBigInt )
  def fieldValueToHex0x(  bi : BigInt     ) : String = bi.unsignedBytes(32).hex0x
  def fieldValueToHex0x( jbi : BigInteger ) : String = fieldValueToHex0x( jbi.toBigInt )
  def fieldValueToBase64Url( bi : BigInt ) : String = Base64.getUrlEncoder.encodeToString(bi.unsignedBytes(32))
  def fieldValueToBase64Url( jbi : BigInteger ) : String = fieldValueToBase64Url( jbi.toBigInt )


