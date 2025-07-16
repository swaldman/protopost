package protopost

import javax.sql.DataSource
import protopost.db.{PgDatabase,PgSchemaManager}

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
    import com.mchange.reauth.*
    val currentSpec = AuthManager.Spec( Authenticator.BCryptVersion.Version2A, 12 )
    val longPasswordStrategyForCurrentOrHistoricalSpec = Map (
      AuthManager.Spec( Authenticator.BCryptVersion.Version2A, 12 ) -> Authenticator.LongPasswordStrategy.Strict
    )
    AuthManager[PosterId](currentSpec, longPasswordStrategyForCurrentOrHistoricalSpec, entropy)

