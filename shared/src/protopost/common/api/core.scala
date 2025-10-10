package protopost.common.api

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import protopost.common.{EmailAddress,Password,PosterId,Protocol,Service}
import java.time.Instant

val LastCharString = "\uDBFF\uDFFD"

object LoginStatus:
  val empty = LoginStatus(0L,0L)
case class LoginStatus( highSecuritySecondsRemaining : Long, lowSecuritySecondsRemaining : Long )

case class EmailPassword( email : EmailAddress, password : Password )

object UpdateValue:
  case class update[+T]( value : T ) extends UpdateValue[T]
  case object `set-to-none` extends UpdateValue[Nothing]
  case object `leave-alone` extends UpdateValue[Nothing]
sealed trait UpdateValue[+T]

case class NewPostRevision(
  postId      : Int,
  contentType : String,
  body        : String
)

case class RetrievedPostRevision(
  postId                : Int,
  timestampEpochSeconds : Long,
  timestampNanos        : Int,
  contentType           : String,
  body                  : String
)

object PostRevisionIdentifier:
  def apply( postId : Int, timestamp : Instant ) : PostRevisionIdentifier =
    apply( postId, timestamp.getEpochSecond(), timestamp.getNano() )
case class PostRevisionIdentifier(
  postId                : Int,
  timestampEpochSeconds : Long,
  timestampNanos        : Int,
)

case class PostMediaUploaded(
  postId : Int,
  path   : String,
  length : Long,
  `type` : Option[String]
)

object RevisionTimestamp:
  def apply( timestamp : Instant ) : RevisionTimestamp =
    apply( timestamp.getEpochSecond(), timestamp.getNano() )
case class RevisionTimestamp(
  timestampEpochSeconds : Long,
  timestampNanos        : Int,
):
  lazy val asInstant : Instant =
    Instant.ofEpochSecond(timestampEpochSeconds, timestampNanos)

case class PostRevisionHistory( postId : Int, revisionTimestampReverseChronological : Seq[RevisionTimestamp] )

case class PostDefinitionCreate(
  destinationSeismicNodeId : Int,
  destinationName          : String,
  owner                    : PosterId,
  title                    : Option[String]  = None,
  postAnchor               : Option[String]  = None,
  sprout                   : Option[Boolean] = None,
  inReplyToSpecifier       : Option[String]  = None,
  authors                  : Seq[String]     = Seq.empty
)

case class PostDefinitionUpdate(
  postId                   : Int,
  title                    : UpdateValue[String]      = UpdateValue.`leave-alone`,
  postAnchor               : UpdateValue[String]      = UpdateValue.`leave-alone`,
  sprout                   : UpdateValue[Boolean]     = UpdateValue.`leave-alone`,
  inReplyToSpecifier       : UpdateValue[String]      = UpdateValue.`leave-alone`,
  authors                  : UpdateValue[Seq[String]] = UpdateValue.`leave-alone`
)

case class PostDefinition(
  postId                   : Int,
  destination              : Destination,
  owner                    : PosterNoAuth,
  title                    : Option[String],
  postAnchor               : Option[String],
  sprout                   : Option[Boolean],
  inReplyToSpecifier       : Option[String],
  publicationAttempted     : Boolean,
  htmlPermalink            : Option[String],
  authors                  : Seq[String]
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
  given Ordering[Destination] = Ordering.by( d => ( d.nickname.getOrElse(LastCharString), d.seismicNode, d.name ) )
case class Destination( seismicNode : SeismicNode, name : String, nickname : Option[String] ):
  def destinationIdentifier = DestinationIdentifier(seismicNode.id,name)

case class DestinationIdentifier( seismicNodeId : Int, name : String )

case class PostIdentifier( destinationIdentifier : DestinationIdentifier, postId : Int )

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

given [T : JsonValueCodec] : JsonValueCodec[UpdateValue[T]] = JsonCodecMaker.make

given JsonValueCodec[LoginStatus] = JsonCodecMaker.make

given JsonValueCodec[EmailPassword] = JsonCodecMaker.make

given JsonValueCodec[PostDefinition] = JsonCodecMaker.make
given JsonValueCodec[PostDefinitionCreate] = JsonCodecMaker.make
given JsonValueCodec[PostDefinitionUpdate] = JsonCodecMaker.make
given given_JsonValueCodec_Set_PostDefinition : JsonValueCodec[Set[PostDefinition]] = JsonCodecMaker.make

given JsonValueCodec[PosterNoAuth] = JsonCodecMaker.make

given JsonValueCodec[SeismicNode] = JsonCodecMaker.make

given JsonValueCodec[Destination] = JsonCodecMaker.make

given JsonValueCodec[DestinationIdentifier]      = JsonCodecMaker.make
given given_JsonValueCodec_Set_DestinationIdentifier : JsonValueCodec[Set[DestinationIdentifier]] = JsonCodecMaker.make

given JsonValueCodec[Set[Destination]] = JsonCodecMaker.make

given given_JsonValueCodec_Option_PostIdentifier : JsonValueCodec[Option[PostIdentifier]] = JsonCodecMaker.make

given JsonValueCodec[NewPostRevision] = JsonCodecMaker.make

given JsonValueCodec[RetrievedPostRevision] = JsonCodecMaker.make

given given_JsonValueCodec_Option_RetrievedPostRevision : JsonValueCodec[Option[RetrievedPostRevision]] = JsonCodecMaker.make

given JsonValueCodec[PostRevisionIdentifier] = JsonCodecMaker.make
given given_JsonValueCodec_Option_PostRevisionIdentifier : JsonValueCodec[Option[PostRevisionIdentifier]] = JsonCodecMaker.make

given JsonValueCodec[RevisionTimestamp]                              = JsonCodecMaker.make
given JsonValueCodec[Tuple2[RevisionTimestamp,NewPostRevision]]      = JsonCodecMaker.make
given JsonValueCodec[List[Tuple2[RevisionTimestamp,NewPostRevision]]] = JsonCodecMaker.make

given JsonValueCodec[PostRevisionHistory] = JsonCodecMaker.make

given JsonValueCodec[PostMediaUploaded] = JsonCodecMaker.make








