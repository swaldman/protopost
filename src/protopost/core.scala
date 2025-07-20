package protopost

import com.mchange.rehash.BCryptHash

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash )
