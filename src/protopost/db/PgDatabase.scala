package protopost.db

import zio.*

import javax.sql.DataSource

import protopost.{EmailAddress, PosterId}

import com.mchange.reauth.*

import com.mchange.sc.sqlutil.*
import com.mchange.sc.zsqlutil.*
import protopost.EmailIsAlreadyRegistered

class PgDatabase( val SchemaManager : PgSchemaManager ):
  val Schema = SchemaManager.LatestSchema

  object txn:
    def createUser( ds : DataSource, authManager : AuthManager[PosterId] )( email : EmailAddress, fullName : String, password : Password ) : Task[PosterId] =
      withConnectionTransactional( ds ): conn =>
        val alreadyExists = Schema.Table.Poster.posterExistsForEmail( conn, email )
        if alreadyExists then
          throw new EmailIsAlreadyRegistered(s"Email '$email' is already registered by an existing user!")
        else
          val posterId = Schema.Sequence.PosterId.selectNext( conn )
          val auth = authManager.initialPasswordHash( posterId, password )
          Schema.Table.Poster.insert( conn, posterId, email, fullName, Some(auth) )
          posterId
  end txn
end PgDatabase
