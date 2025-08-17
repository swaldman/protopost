package protopost

import javax.sql.DataSource
import protopost.crypto.BouncyCastleSecp256r1
import protopost.db.{PgDatabase,PgSchemaManager}
import protopost.identity.{ES256LocalIdentity,Location,Protocol,Service}
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

  lazy val inProduction =
    val key = ExternalConfig.Key.`protopost.mode.production`
    externalConfig
      .get( key )
      .map( java.lang.Boolean.parseBoolean )
      .getOrElse( throw new MissingConfig(s"Please set mandatory config key '$key'." ) )

  lazy val authManager =
    import com.mchange.rehash.*
    val currentSpec = AuthManager.Spec( Authenticator.BCryptVersion.Version2A, 12 )
    val longPasswordStrategyForCurrentOrHistoricalSpec = Map (
      AuthManager.Spec( Authenticator.BCryptVersion.Version2A, 12 ) -> Authenticator.LongPasswordStrategy.Strict
    )
    AuthManager[PosterId](currentSpec, longPasswordStrategyForCurrentOrHistoricalSpec, entropy)

  lazy val localIdentity =
    val location =
      externalConfig.get(ExternalConfig.Key.`protopost.server.url`) match
        case Some(url) => Location.Simple(url)
        case None      => Location.DefaultProtopost
    val pvtKeyHexKey = ExternalConfig.Key.`protopost.server.private-key-hex`
    val hex =
      externalConfig
        .get(pvtKeyHexKey)
        .getOrElse( throw new MissingConfig(s"Please set config key '$pvtKeyHexKey'. Cannot establish server identity with '$pvtKeyHexKey' unset.") )
    val privateKey = BouncyCastleSecp256r1.privateKeyFromHex( hex )
    val publicKey = BouncyCastleSecp256r1.publicKeyFromPrivate( privateKey )
    ES256LocalIdentity( location, Service.protopost, privateKey, publicKey ) 
