package protopost

import protopost.api.PosterNoAuth
import com.mchange.rehash.BCryptHash

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash ):
  lazy val toPosterNoAuth : PosterNoAuth = PosterNoAuth( id, email, fullName )
