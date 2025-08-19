package protopost.client

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobalScope
object Globals extends js.Object {
  val protopostLocation : String = js.native
}

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

