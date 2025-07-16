package protopost

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash )
