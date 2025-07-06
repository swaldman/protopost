package protopost

import protopost.db.PosterId

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash )
