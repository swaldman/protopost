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
enum LoginLevel( val colorClass : String ):
  case high extends LoginLevel("logged-in-high")
  case low  extends LoginLevel("logged-in-low")
  case none extends LoginLevel("logged-in-none")

object UserLocation:
  object TableOfContents extends UserLocation
  object NewPostLaunchpad extends UserLocation
  case class ExistingPost( id : Int ) extends UserLocation
  object Profile extends UserLocation
sealed trait UserLocation  

