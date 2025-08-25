package protopost

import protopost.api.PosterNoAuth
import protopost.identity.{Location,Protocol,Service}

import com.mchange.rehash.BCryptHash

import com.mchange.cryptoutil.given

import scala.collection.immutable

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash ):
  lazy val toPosterNoAuth : PosterNoAuth = PosterNoAuth( id, email, fullName )

case class SeismicNodeWithId( id : Int, algcrv : String, pubkey : immutable.ArraySeq[Byte], protocol : Protocol, host : String, port : Int ):
  lazy val location : Location.Simple = Location.Simple( protocol, host, port )
  lazy val identifier = s"${Service.seismic}[${algcrv}]${pubkey.hex0x}"
  lazy val identifierWithLocation = s"${identifier}:${location.toUrl}"
