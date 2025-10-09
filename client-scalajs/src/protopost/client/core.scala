package protopost.client

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import protopost.common.api.{LoginStatus,PostDefinition}

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

@js.native
trait CkEditorDocument extends js.Object:
  def on( eventType : String, callback : js.Function1[js.Object,Unit]) : Unit = js.native

@js.native
trait CkEditorModel extends js.Object:
  val document : CkEditorDocument = js.native

@js.native
@JSGlobal
class CkEditor extends js.Any:
  def getData()                              : String = js.native
  def setData( htmlText : String )           : Unit   = js.native
  def enableReadOnlyMode( lockId : String )  : Unit   = js.native
  def disableReadOnlyMode( lockId : String ) : Unit   = js.native
  val model : CkEditorModel = js.native

  def on( eventType : String, callback : js.Function1[Object,Unit]) : Unit   = js.native

@js.native
@JSGlobalScope
object Globals extends js.Object:
  val protopostLocation : String = js.native
  def bindCkEditor( containerId : String ) : js.Promise[CkEditor] = js.native

@js.native
@JSGlobal
object DOMPurify extends js.Object:
  def sanitize(raw : String) : String = js.native

@js.native
@JSGlobal
object marked extends js.Object:
  def parse(markdownText : String) : String = js.native

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
  case `text-and-preview` extends Composer( "Text and preview (plaintext, markdown, html)" )
  case `WYSIWYG`          extends Composer( "WYSIWYG (html)" )

object PostContent:
  val default = PostContent("text/plain","")
case class PostContent( contentType : String, text : String )

// case class PostInProgress( id : Int, dirtyToLocalStorage : Boolean, dirtyToServer : Boolean, fetchCurrentText : () => PostContent )

given JsonValueCodec[Composer]    = JsonCodecMaker.make
given JsonValueCodec[PostContent] = JsonCodecMaker.make


