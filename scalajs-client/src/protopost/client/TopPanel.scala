package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.util.{Success,Failure}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
import scala.scalajs.js.timers.*

import protopost.api.{LoginStatus, given}
import protopost.client.util.epochSecondsNow

object TopPanel:
  private val LoginStatusUpdateIntervalMsecs         = 6000
  private val LoginStatusUpdateHardUpdateProbability = 1d/600 // so we hard update about once and hour

  def create(protopostLocation : Uri) : HtmlElement =

    val loginStatusVar : Var[Option[(LoginStatus, Long)]] = Var(None)
    var loginStatusHandle : Option[SetIntervalHandle] = None

    val loginLevelSignal : Signal[Option[LoginLevel]] = loginStatusVar.signal.map( _.map( (ls,_) => LoginLevel.fromLoginStatus(ls) ) )

    val locationVar : Var[UserLocation] = Var(UserLocation.Profile)

    val loggedInLocationSignal = locationVar.signal.combineWithFn(loginLevelSignal): ( loc, mbLevel ) =>
      mbLevel match
        case Some( LoginLevel.high | LoginLevel.low ) => Some(loc)
        case _ => None

    val loginForm = LoginForm.create(protopostLocation, loginStatusVar, loginLevelSignal)
    val profilePanel = ProfilePanel.create(protopostLocation, loginLevelSignal)

    def updateLoginStatus() : Unit =
      // on initial mount, the update seems sometimes to skip,
      // perhaps a race condition as mount is not completed?
      //
      // so we default to a hard update if update doesn't work out.
      var hardUpdate = true 
      loginStatusVar.update: optTup =>
        optTup match
          case Some(Tuple2(ls,lastUpdated)) =>
            val now = epochSecondsNow()
            val elapsedSeconds = now - lastUpdated
            val newStatus = LoginStatus( ls.highSecuritySecondsRemaining - elapsedSeconds, ls.lowSecuritySecondsRemaining - elapsedSeconds )
            hardUpdate = math.random < LoginStatusUpdateHardUpdateProbability
            Some(Tuple2(newStatus, now))
          case None =>
            hardUpdate = true
            None
      if hardUpdate then
        protopost.client.util.sttp.hardUpdateLoginStatus(protopostLocation, Client.backend, loginStatusVar)

    def maintainLoginStatus() : Unit =
      updateLoginStatus()
      if loginStatusHandle == None then
        val handle =
          setInterval(LoginStatusUpdateIntervalMsecs):
            updateLoginStatus()
        loginStatusHandle = Some( handle )

    def retireLoginStatus() : Unit =
      if loginStatusHandle != null && loginStatusHandle != None then
        clearInterval( loginStatusHandle.get )
        loginStatusHandle = None

    div(
      onMountCallback { _ => maintainLoginStatus() },
      onUnmountCallback { _ => retireLoginStatus() },
      idAttr("top"),
      cls <-- loginLevelSignal.map( _.fold("logged-in-unknown")( _.colorClass ) ),
      loginForm.amend(
        display <-- loggedInLocationSignal.map( _.fold("flex")(_ => "none") )
      ),
      profilePanel.amend(
        display <-- loggedInLocationSignal.map( opt => if opt == Some(UserLocation.Profile) then "block" else "none" )
      )
    )

