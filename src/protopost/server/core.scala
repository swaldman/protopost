package protopost.server

import protopost.{EmailAddress,PosterId}
import protopost.api
import protopost.common.{Protocol,Service}
import protopost.identity.Location

import com.mchange.rehash.BCryptHash

import com.mchange.cryptoutil.given

import scala.collection.immutable

val LoggingApi = logadapter.zio.ZApi( logadapter.log4j2.Api )

case class PostDefinitionRaw(
  postId                   : Int,
  destinationSeismicNodeId : Int,
  destinationName          : String,
  owner                    : PosterId,
  title                    : Option[String]  = None,
  postAnchor               : Option[String]  = None,
  sprout                   : Option[Boolean] = None,
  inReplyToHref            : Option[String]  = None,
  inReplyToMimeType        : Option[String]  = None,
  inReplyToGuid            : Option[String]  = None,
  publicationAttempted     : Boolean         = false,
  htmlPermalink            : Option[String]  = None
)

case class PosterWithAuth( id : PosterId, email : EmailAddress, fullName : String, auth : BCryptHash ):
  lazy val toApiPosterNoAuth : api.PosterNoAuth = api.PosterNoAuth( id, email, fullName )

case class SeismicNodeWithId( id : Int, algcrv : String, pubkey : immutable.ArraySeq[Byte], protocol : Protocol, host : String, port : Int ):
  lazy val location : Location.Simple = Location.Simple( protocol, host, port )
  lazy val identifier = s"${Service.seismic}[${algcrv}]${pubkey.hex0x}"
  lazy val identifierWithLocation = s"${identifier}:${location.toUrl}"
  lazy val toApiSeismicNode : api.SeismicNode = api.SeismicNode(id, algcrv, pubkey.hex0x, protocol, host, port)

