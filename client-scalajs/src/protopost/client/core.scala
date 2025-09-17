package protopost.client

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import protopost.common.api.{LoginStatus,PostDefinition}

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

@js.native
@JSGlobalScope
object Globals extends js.Object {
  val protopostLocation : String = js.native
}

val ReverseChronologicalPostDefinitions = Ordering.by[PostDefinition,Int]( pd => -pd.postId ) // reverse chronological of post creation, since id's are allocated chronologically

object LoginLevel:
  def fromLoginStatus(loginStatus : LoginStatus) : LoginLevel =
    if loginStatus.highSecuritySecondsRemaining > 0 then
      LoginLevel.high
    else if loginStatus.lowSecuritySecondsRemaining > 0 then
      LoginLevel.low
    else
      LoginLevel.none
enum LoginLevel( val cssColor : String, val isLoggedIn : Boolean ):
  case unknown extends LoginLevel("gray",false)
  case none    extends LoginLevel("red",false)
  case low     extends LoginLevel("cyan",true)
  case high    extends LoginLevel("green",true)

enum Tab( val label : String ):
  //case newPost  extends Tab("new post")
  case destinationsAndPosts extends Tab("destinations and posts")
  case currentPost  extends Tab("current post")
  case profile  extends Tab("profile")

enum Composer( val label : String ):
  case `text-and-preview` extends Composer("Text and preview (plaintext, markdown, html)")
  case `WYSIWYG`          extends Composer( "WYSIWYG (html)" )

case class PostInProgress( id : Int, dirtyToLocalStorage : Boolean, dirtyToServer : Boolean, fetchCurrentText : () => String )

given JsonValueCodec[Tab]      = JsonCodecMaker.make
given JsonValueCodec[Composer] = JsonCodecMaker.make

