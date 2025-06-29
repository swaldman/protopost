package protopost.crypto

import java.security.Security

object BouncyCastle:
  val Provider =
    import org.bouncycastle.jce.provider.BouncyCastleProvider;
    val p = new BouncyCastleProvider()
    Security.addProvider(p);
    p

  val ProviderName = "BC"
  
