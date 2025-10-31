package protopost.server

import protopost.common.{PosterId,Protocol,Service}
import protopost.server.{ExternalConfig,ConfigProperties}
import protopost.server.crypto.BouncyCastleSecp256r1
import protopost.server.db.{PgDatabase,PgSchemaManager}
import protopost.server.exception.MissingConfig
import protopost.server.identity.{LocalIdentity,Location}
import java.security.interfaces.{ECPrivateKey,ECPublicKey}
import javax.sql.DataSource
import scala.collection.mutable
import com.auth0.jwk.{Jwk,JwkProvider,JwkProviderBuilder}

import sttp.client4.httpclient.zio.SttpClient

import ConfigProperties.{p as props}
import scala.util.Try
import com.mchange.mailutil.Smtp

import com.mchange.conveniences.javautil.*
import scala.jdk.CollectionConverters.*

import LoggingApi.*

class AppResources( val configProperties : ConfigProperties, val sttpClient : SttpClient ) extends SelfLogging:

  lazy val dataSource : DataSource =
    import com.mchange.v2.c3p0.*
    val nascent = new ComboPooledDataSource()
    DataSources.overwriteC3P0PrefixedProperties( nascent, props(configProperties) )
    nascent

  lazy val externalConfig : ExternalConfig =  ExternalConfig.fromProperties( props(configProperties) )

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
        case None if inProduction => Location.DefaultProtopost
        case None =>
          val key = ExternalConfig.Key.`protopost.api.local.port`
          val localApiPort = externalConfig.get( key ).getOrElse( throw MissingConfig( key.toString ) ).toInt
          Location.DefaultProtopost.copy( port = localApiPort )
    val pvtKeyHexKey = ExternalConfig.Key.`protopost.server.private-key-hex`
    val hex =
      externalConfig
        .get(pvtKeyHexKey)
        .getOrElse( throw new MissingConfig(s"Please set config key '$pvtKeyHexKey'. Cannot establish server identity with '$pvtKeyHexKey' unset.") )
    val privateKey = BouncyCastleSecp256r1.privateKeyFromHex( hex )
    val publicKey = BouncyCastleSecp256r1.publicKeyFromPrivate( privateKey )
    LocalIdentity.ES256( location, Service.protopost, privateKey, publicKey )

  lazy val optionalMailConfig =
    val mbFromAddress = externalConfig.get( ExternalConfig.Key.`protopost.server.mail.from-address` ).flatMap( address => Try( Smtp.Address(address) ).toOption )
    if mbFromAddress.isEmpty then
      WARNING.log( s"Config property ${ExternalConfig.Key.`protopost.server.mail.from-address`} has not been set or is invalid; mailing of drafts will not be supported." )
    def mbSmtpContext =
      val out = Try( Smtp.Context( (Smtp.Context.defaultProperties().asScala.toMap ++ props(configProperties).asScala.toMap).toProperties, sys.env ) ).toOption
      if out.isEmpty then
        WARNING.log( s"SMTP configuration properties are not present; mailing of drafts will not be supported." )
      out
    for
      fromAddress <- mbFromAddress
      smtpContext <- mbSmtpContext
    yield
      MailConfig( fromAddress, smtpContext )

  object jwkProviders:
    private val innerMap : mutable.Map[Location.Simple,JwkProvider] = mutable.HashMap.empty[Location.Simple,JwkProvider]
    private def get( location : Location.Simple ) : JwkProvider = this.synchronized:
      innerMap.getOrElseUpdate( location, new JwkProviderBuilder(location.toUrl).build() )
    def get( location : Location.Simple, service : Service ) : Option[Jwk] =
      Option( this.get( location ).get( service.toString ) )
