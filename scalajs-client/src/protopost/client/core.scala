package protopost.client

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import protopost.api.LoginStatus

@js.native
@JSGlobalScope
object Globals extends js.Object {
  val protopostLocation : String = js.native
}

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




val UnknownLoginStatusColor = "gray"

/*
object UserLocation:
  object TableOfContents extends UserLocation
  object NewPostLaunchpad extends UserLocation
  case class ExistingPost( id : Int ) extends UserLocation
  object Profile extends UserLocation
sealed trait UserLocation
*/

enum Tab( val label : String ):
  //case newPost  extends Tab("new post")
  case destinationsAndPosts extends Tab("destinations and posts")
  case currentPost  extends Tab("current post")
  case profile  extends Tab("profile")
