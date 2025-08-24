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

import protopost.api.{LoginStatus, PosterNoAuth, given}
import protopost.client.util.epochSecondsNow

object TopPanel:
  private val LoginStatusUpdateIntervalMsecs         = 6000
  private val LoginStatusUpdateHardUpdateProbability = 1d/600 // so we hard update about once and hour

  def create(protopostLocation : Uri) : HtmlElement =

    // ( LoginStatus, time last updated in epoch seconds )
    val loginStatusVar : Var[Option[(LoginStatus, Long)]] = Var(None)
    var loginStatusHandle : Option[SetIntervalHandle] = None

    val loginLevelSignal : Signal[LoginLevel] = loginStatusVar.signal.map( _.fold(LoginLevel.unknown)( (ls,_) => LoginLevel.fromLoginStatus(ls) ) )
    val loginLevelChangeEvents = loginLevelSignal.changes.distinct
    val posterNoAuthVar : Var[Option[PosterNoAuth]] = Var(None)

    val locationVar : Var[UserLocation] = Var(UserLocation.Profile)

    val loggedInLocationSignal = locationVar.signal.combineWithFn(loginLevelSignal): ( loc, level ) =>
      level match
        case LoginLevel.high | LoginLevel.low => Some(loc)
        case _ => None


    val loginObserver = Observer[LoginLevel]: level =>
      println(s"loginObserver - level: ${level}")
      util.sttp.setOptionalVarFromApiGetResult[PosterNoAuth]( protopostLocation.addPath("protopost", "poster-info"), Client.backend, posterNoAuthVar )

    val loginForm = LoginForm.create( protopostLocation, loginStatusVar, loginLevelSignal, loginLevelChangeEvents )
    val profilePanel = ProfilePanel.create(loginLevelChangeEvents,loginObserver,posterNoAuthVar)

    val logoutSubmitter = Observer[dom.MouseEvent]: tup =>
      val transformation : LoginStatus => Option[(LoginStatus,Long)] = loginStatus => Some(Tuple2(loginStatus, epochSecondsNow()))
      val request = basicRequest.post( protopostLocation.addPath("protopost","logout") ).response(asJson[LoginStatus])
      util.sttp.setVarFromTransformedApiResult( request, Client.backend, loginStatusVar, transformation )

    def updateLoginStatus() : Unit =
      // on initial mount, the update seems sometimes to skip,
      // perhaps a race condition as mount is not completed?
      //
      // so we default to a hard update if update doesn't work out.
      var hardUpdate = true
      loginStatusVar.update: optTup =>
        //println("updateLoginStatus() -- optTup: " + optTup);
        optTup match
          case Some(Tuple2(ls,lastUpdated)) =>
            val now = epochSecondsNow()
            val elapsedSeconds = now - lastUpdated
            val newStatus = LoginStatus( math.max(0,ls.highSecuritySecondsRemaining - elapsedSeconds), math.max(0,ls.lowSecuritySecondsRemaining - elapsedSeconds) )
            hardUpdate = math.random < LoginStatusUpdateHardUpdateProbability
            if hardUpdate then
              println(s"hardUpdate: $hardUpdate")
            Some(Tuple2(newStatus, now))
          case None =>
            hardUpdate = true
            None
      if hardUpdate then
        //println("Making hard login status request...")
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
      // very annoyingly, there's not an easy way to set grid- and hover-related style elements (beyond display.grid itself) in laminar
      styleTag(
        """
        |#app-panel {
        |  display: grid;
        |  grid-template-columns: 1fr;
        |  grid-template-rows: auto 1fr auto;
        |}
        |#logout-pane {
        |  color: blue;
        |  transition: background-color 0.3s;
        |}
        |#logout-pane:hover {
        |  color: red;
        |}
        """.stripMargin
      ),
      borderColor <-- loginLevelSignal.map( _.cssColor ),
      width.percent(100),
      height.percent(100),
      borderStyle.solid,
      borderWidth.px(3),
      loginForm.amend(
        display <-- loggedInLocationSignal.map( _.fold("flex")(_ => "none") )
      ),
      div(
        idAttr("app-panel"),
        width.percent(100),
        height.percent(100),
        display <-- loggedInLocationSignal.map {
            case Some(_) => "grid"
            case None => "none"
          },
        div(
          idAttr("app-top-panel"),
          display.flex,
          flexDirection.rowReverse,
          //backgroundColor.black,
          paddingTop.px(4),
          paddingRight.px(4),
          fontSize.pt(9),
          a(
            idAttr("logout-pane"),
            "logout",
            onClick --> logoutSubmitter, //{ event => dom.window.alert("logout") },
            cursor("default"),
          )
        ),
        div(
          // card panel
          idAttr("card-panel"),
          profilePanel.amend(
            display <-- loggedInLocationSignal.map( opt => if opt == Some(UserLocation.Profile) then "block" else "none" )
          ),
          //marginTop.auto,
          marginBottom.auto,
        ),
        div(
          // tab panel
          idAttr("tab-panel"),
          display.flex,
          flexDirection.row,
          justifyContent.spaceAround,
          createTab("New Post"),
          createTab("All Posts"),
          createTab("Profile"),
          borderWidth.px(5),
          borderColor.black,
        )
      )
    )

  def createTab( label : String ) : HtmlElement =
    div(
      cls("tab-pane"),
      label,
      textAlign.center,
    )
