package protopost

import com.mchange.reauth.BCryptHash

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash )
