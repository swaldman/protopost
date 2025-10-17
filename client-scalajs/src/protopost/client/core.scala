package protopost.client

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import protopost.common.api.{LoginStatus,PostDefinition}

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

/*
// modified from https://www.scala-js.org/doc/interoperability/global-scope.html
val jsGlobalObject: js.Dynamic =
  import js.Dynamic.{global => g}
  if js.typeOf(g.global) != "undefined" && (g.global.Object eq g.Object) then
    // Node.js environment detected
    g.global
  else
    // In all other well-known environment, we can use the global `this`
    js.special.fileLevelThis.asInstanceOf[js.Dynamic]
*/

@js.native
@JSGlobalScope
object Globals extends js.Object:
  val protopostLocation : String = js.native
  var protopostCurrentPostId : Int = js.native
  var protopostExternalJsConfig : js.Object = js.native
  def html_beautify(html : String ) : String = js.native

@js.native
@JSGlobal
object DOMPurify extends js.Object:
  def sanitize(raw : String) : String = js.native

@js.native
@JSGlobal
object marked extends js.Object:
  def parse(markdownText : String) : String = js.native

object ProtopostExternalJsConfig:
  val default = ProtopostExternalJsConfig(
    ckeditorLoadedImagesDefaultToWidth100Percent = true
  )
case class ProtopostExternalJsConfig(
  val ckeditorLoadedImagesDefaultToWidth100Percent : Boolean
):
  def toJsObject : js.Object = js.Dynamic.literal( ckeditorLoadedImagesDefaultToWidth100Percent = ckeditorLoadedImagesDefaultToWidth100Percent )
end ProtopostExternalJsConfig

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

enum Composer( val label : String ):
  case `WYSIWYG`          extends Composer( "WYSIWYG (html)" )
  case `text-and-preview` extends Composer( "Text and preview (plaintext, markdown, html)" )

object PostContent:
  val default = PostContent("text/plain","")
case class PostContent( contentType : String, text : String )

// case class PostInProgress( id : Int, dirtyToLocalStorage : Boolean, dirtyToServer : Boolean, fetchCurrentText : () => PostContent )

given JsonValueCodec[ProtopostExternalJsConfig] = JsonCodecMaker.make

given JsonValueCodec[Composer]    = JsonCodecMaker.make
given JsonValueCodec[PostContent] = JsonCodecMaker.make


