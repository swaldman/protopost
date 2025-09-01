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

object UpdateValue:
  case class Value[+T]( value : T ) extends UpdateValue[T]
  case object Null extends UpdateValue[Nothing]
sealed trait UpdateValue[+T]

case class PostDefinitionCreate(
  destinationSeismicNodeId : Int,
  destinationName          : String,
  owner                    : PosterId,
  title                    : Option[String]  = None,
  postAnchor               : Option[String]  = None,
  sprout                   : Option[Boolean] = None,
  inReplyToHref            : Option[String]  = None,
  inReplyToMimeType        : Option[String]  = None,
  inReplyToGuid            : Option[String]  = None,
  authors                  : Seq[String]     = Seq.empty
)

case class PostDefinitionUpdate(
  postId                   : Int,
  title                    : Option[UpdateValue[String]],
  postAnchor               : Option[UpdateValue[String]],
  sprout                   : Option[UpdateValue[Boolean]],
  inReplyToHref            : Option[UpdateValue[String]],
  inReplyToMimeType        : Option[UpdateValue[String]],
  inReplyToGuid            : Option[UpdateValue[String]],
  authors                  : Option[Seq[String]] //cannot be NULL
)

case class PostDefinition(
  postId                   : Int,
  destination              : Destination,
  owner                    : PosterNoAuth,
  title                    : Option[String],
  postAnchor               : Option[String],
  sprout                   : Option[Boolean],
  inReplyToHref            : Option[String],
  inReplyToMimeType        : Option[String],
  inReplyToGuid            : Option[String],
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
  given Ordering[Destination] = Ordering.by( d => ( d.seismicNode, d.name ) )
case class Destination( seismicNode : SeismicNode, name : String )

case class DestinationIdentifier( seismicNodeId : Int, name : String )

case class PostIdentifier( destinationIdentifier : DestinationIdentifier, postId : Int )

object DestinationNickname:
  given Ordering[DestinationNickname] = Ordering.by( dn => (dn.destination, dn.nickname.getOrElse(LastCharString)) )
case class DestinationNickname( destination : Destination, nickname : Option[String] ):
  def destinationIdentifier = DestinationIdentifier(destination.seismicNode.id,destination.name)

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

given [T : JsonValueCodec] : JsonValueCodec[UpdateValue[T]] = new JsonValueCodec[UpdateValue[T]]:
  val baseCodec = summon[JsonValueCodec[T]]
  def decodeValue(in : JsonReader, default : UpdateValue[T]) : UpdateValue[T] =
    val tOrNull = baseCodec.decodeValue(in,baseCodec.nullValue)
    if tOrNull == null then UpdateValue.Null else UpdateValue.Value(tOrNull)
  def encodeValue(x : UpdateValue[T], out: JsonWriter): Unit =
    val baseCodec = summon[JsonValueCodec[T]]
    x match
      case protopost.api.UpdateValue.Value( value ) => baseCodec.encodeValue( value, out )
      case protopost.api.UpdateValue.Null => baseCodec.nullValue
  def nullValue : UpdateValue[T] = UpdateValue.Null

given JsonValueCodec[LoginStatus] = JsonCodecMaker.make

given JsonValueCodec[EmailPassword] = JsonCodecMaker.make

//given JsonValueCodec[UpdateValue.Value[String]] = JsonCodecMaker.make
//given JsonValueCodec[UpdateValue.Value[Boolean]] = JsonCodecMaker.make
//given JsonValueCodec[UpdateValue.Null.type] = JsonCodecMaker.make
//given JsonValueCodec[UpdateValue[String]] = JsonCodecMaker.make
//given JsonValueCodec[UpdateValue[Boolean]] = JsonCodecMaker.make

given JsonValueCodec[PostDefinition] = JsonCodecMaker.make
given JsonValueCodec[PostDefinitionCreate] = JsonCodecMaker.make
given JsonValueCodec[PostDefinitionUpdate] = JsonCodecMaker.make
given given_JsonValueCodec_Set_PostDefinition : JsonValueCodec[Set[PostDefinition]] = JsonCodecMaker.make

given JsonValueCodec[PosterNoAuth] = JsonCodecMaker.make

given JsonValueCodec[SeismicNode] = JsonCodecMaker.make

given JsonValueCodec[Destination] = JsonCodecMaker.make

given JsonValueCodec[DestinationIdentifier] = JsonCodecMaker.make

given JsonValueCodec[DestinationNickname] = JsonCodecMaker.make

given given_JsonValueCodec_Set_DefinitionNickname : JsonValueCodec[Set[DestinationNickname]] = JsonCodecMaker.make
