package protopost.api

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import protopost.{EmailAddress,Password,PosterId}
import protopost.common.{Protocol,Service}

val LastCharString = "\uDBFF\uDFFD"

object LoginStatus:
  val empty = LoginStatus(0L,0L)
case class LoginStatus( highSecuritySecondsRemaining : Long, lowSecuritySecondsRemaining : Long )

case class EmailPassword( email : EmailAddress, password : Password )

case class PostDefinition(
  postId                   : Int,
  destinationSeismicNodeId : Int,
  destinationName          : String,
  owner                    : PosterId,
  contentType              : String,
  title                    : Option[String],
  postAnchor               : Option[String],
  sprout                   : Option[Boolean],
  inReplyToHref            : Option[String],
  inReplyToMimeType        : Option[String],
  inReplyToGuid            : Option[String],
  publicationAttempted     : Boolean,
  publicationConfirmed     : Boolean
)

case class PostUpdatables(
  title                    : Option[String],
  postAnchor               : Option[String],
  sprout                   : Option[Boolean],
  inReplyToHref            : Option[String],
  inReplyToMimeType        : Option[String],
  inReplyToGuid            : Option[String]
)

case class PosterNoAuth( id : PosterId, email : EmailAddress, fullName : String )

object SeismicNode:
  given Ordering[SeismicNode] = Ordering.by( sn => ( sn.algcrv, sn.publicKeyHex0x, sn.protocol.toString, sn.host, sn.port ) )
case class SeismicNode( id : Int, algcrv : String, publicKeyHex0x : String, protocol : Protocol, host : String, port : Int ):
  lazy val locationUrl =
    val portStr = if port == protocol.defaultPort then "" else s":${port}"
    s"${protocol}://${host}${portStr}/"
  lazy val identifierWithLocation =
    s"${Service.seismic}[${algcrv}]${publicKeyHex0x}:${locationUrl}"

object Destination:
  given Ordering[Destination] = Ordering.by( d => ( d.seismicNode, d.name ) )
case class Destination( seismicNode : SeismicNode, name : String )

object DestinationNickname:
  given Ordering[DestinationNickname] = Ordering.by( dn => (dn.destination, dn.nickname.getOrElse(LastCharString)) )
case class DestinationNickname( destination : Destination, nickname : Option[String] )

// json codecs -- jsoniter-scala
given JsonValueCodec[EmailAddress] = new JsonValueCodec[EmailAddress]:
  def decodeValue(in : JsonReader, default : EmailAddress) : EmailAddress = EmailAddress(in.readString(null))
  def encodeValue(x : EmailAddress, out: JsonWriter): Unit = out.writeVal(x.toString)
  def nullValue : EmailAddress = null.asInstanceOf[EmailAddress]

given JsonValueCodec[Password] = new JsonValueCodec[Password]:
  def decodeValue(in : JsonReader, default : Password) : Password = Password(in.readString(null))
  def encodeValue(x : Password, out: JsonWriter): Unit = out.writeVal(x.toString)
  def nullValue : Password = null.asInstanceOf[Password]

given JsonValueCodec[PosterId] = new JsonValueCodec[PosterId]:
  import PosterId.i
  def decodeValue(in : JsonReader, default : PosterId) : PosterId = PosterId(in.readInt())
  def encodeValue(x : PosterId, out: JsonWriter): Unit = out.writeVal(i(x))
  def nullValue : PosterId = (-1).asInstanceOf[PosterId]

given JsonValueCodec[LoginStatus] = JsonCodecMaker.make

given JsonValueCodec[EmailPassword] = JsonCodecMaker.make

given JsonValueCodec[PostDefinition] = JsonCodecMaker.make

given JsonValueCodec[PostUpdatables] = JsonCodecMaker.make

given JsonValueCodec[PosterNoAuth] = JsonCodecMaker.make

given JsonValueCodec[SeismicNode] = JsonCodecMaker.make

given JsonValueCodec[Destination] = JsonCodecMaker.make

given JsonValueCodec[DestinationNickname] = JsonCodecMaker.make

given JsonValueCodec[Set[DestinationNickname]] = JsonCodecMaker.make
