package protopost.api

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

import protopost.{EmailAddress,Password,PosterId}

object LoginStatus:
  val empty = LoginStatus(0L,0L)
case class LoginStatus( highSecuritySecondsRemaining : Long, lowSecuritySecondsRemaining : Long )
case class EmailPassword( email : EmailAddress, password : Password )
case class PosterNoAuth( id : PosterId, email : EmailAddress, fullName : String )


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
given JsonValueCodec[LoginStatus]   = JsonCodecMaker.make
given JsonValueCodec[EmailPassword] = JsonCodecMaker.make
given JsonValueCodec[PosterNoAuth] = JsonCodecMaker.make
