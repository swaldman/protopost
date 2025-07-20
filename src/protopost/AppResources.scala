package protopost

import javax.sql.DataSource
import protopost.crypto.BouncyCastleSecp256r1
import protopost.db.{PgDatabase,PgSchemaManager}
import java.security.interfaces.ECPrivateKey
import java.security.interfaces.ECPublicKey

class AppResources( val configProperties : ConfigProperties ):

  lazy val dataSource : DataSource =
    import com.mchange.v2.c3p0.*
    val nascent = new ComboPooledDataSource()
    DataSources.overwriteC3P0PrefixedProperties( nascent, configProperties.props )
    nascent

  lazy val externalConfig : ExternalConfig =  ExternalConfig.fromProperties( configProperties.props )

  lazy val schemaManager : PgSchemaManager = new PgSchemaManager( externalConfig )

  lazy val database : PgDatabase = new PgDatabase( schemaManager )

  lazy val entropy = new java.security.SecureRandom

  lazy val authManager =
    import com.mchange.rehash.*
    val currentSpec = AuthManager.Spec( Authenticator.BCryptVersion.Version2A, 12 )
    val longPasswordStrategyForCurrentOrHistoricalSpec = Map (
      AuthManager.Spec( Authenticator.BCryptVersion.Version2A, 12 ) -> Authenticator.LongPasswordStrategy.Strict
    )
    AuthManager[PosterId](currentSpec, longPasswordStrategyForCurrentOrHistoricalSpec, entropy)

  lazy val keyPair : ( ECPrivateKey, ECPublicKey ) =
    val hex = externalConfig.get( ExternalConfig.Key.`protopost.server.private-key-hex` ).getOrElse( throw new MissingConfig( s"${ExternalConfig.Key.`protopost.server.private-key-hex`} required, not set." ) )
    val privateKey = BouncyCastleSecp256r1.privateKeyFromHex( hex )
    val publicKey  = BouncyCastleSecp256r1.publicKeyFromPrivate( privateKey )
    ( privateKey, publicKey )
