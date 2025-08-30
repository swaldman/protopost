package protopost.client

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import protopost.api.{LoginStatus,PostDefinition}

@js.native
@JSGlobalScope
object Globals extends js.Object {
  val protopostLocation : String = js.native
}

val ReverseChronologicalPostDefinitions = Ordering.by[PostDefinition,Int]( pd => -pd.postId ) // reverse chronological of post creation, since id's are allocated chronologically

// stolen from conveniences...
private def commaListXXX(xxx : String)( seq : Seq[String] ) : Option[String] =
  seq.length match
    case 0 => None
    case 1 => Some( seq.head )
    case 2 => Some( seq.head + s" $xxx " + seq.last )
    case n =>
      val anded = seq.init :+ s"$xxx ${seq.last}"
      Some( anded.mkString(", ") )

def commaListAnd( seq : Seq[String] ) : Option[String] = commaListXXX("and")(seq)
def commaListOr( seq : Seq[String] )  : Option[String] = commaListXXX("or")(seq)


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

case class PostInProgress( id : Int, dirtyToLocalStorage : Boolean, dirtyToServer : Boolean, fetchCurrentText : () => String )
