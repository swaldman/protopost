package protopost.db

import zio.*

import javax.sql.DataSource

import protopost.{BCryptHash, EmailAddress, Password}

import com.mchange.sc.sqlutil.*
import com.mchange.sc.zsqlutil.*

class PgDatabase( val SchemaManager : PgSchemaManager ):
  val Schema = SchemaManager.LatestSchema

  object Transaction:
    def createUser( ds : DataSource )( email : EmailAddress, fullName : String, auth : BCryptHash ) : Task[Unit] =
      withConnectionTransactional( ds ): conn =>
        val posterId = Schema.Sequence.PosterId.selectNext( conn )
        Schema.Table.Poster.insert( conn, posterId, email, fullName, auth )

  end Transaction
end PgDatabase
