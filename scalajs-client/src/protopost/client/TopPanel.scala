package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.collection.immutable
import scala.util.{Success,Failure}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
import scala.scalajs.js.timers.*

import protopost.api.{DestinationNickname, LoginStatus, PosterNoAuth, given}
import protopost.client.util.epochSecondsNow

import Client.TinyLinkFontSize
import scala.util.control.NonFatal

object TopPanel:
  private final val LoginStatusUpdateIntervalMsecs         = 6000
  private final val LoginStatusUpdateHardUpdateProbability = 1d/600 // so we hard update about once and hour

  private final val TopPanelMargin = 2 //2px

  def create(protopostLocation : Uri) : HtmlElement =
    val backend : WebSocketBackend[scala.concurrent.Future] = FetchBackend()

    // ( LoginStatus, time last updated in epoch seconds )
    val loginStatusVar : Var[Option[(LoginStatus, Long)]] = Var(None)
    var loginStatusHandle : Option[SetIntervalHandle] = None

    val loginLevelSignal : Signal[LoginLevel] = loginStatusVar.signal.map( _.fold(LoginLevel.unknown)( (ls,_) => LoginLevel.fromLoginStatus(ls) ) )
    val loginLevelChangeEvents = loginLevelSignal.changes.distinct
    val posterNoAuthVar : Var[Option[PosterNoAuth]] = Var(None)
    val destinationsVar : Var[immutable.SortedSet[DestinationNickname]] = Var( immutable.SortedSet.empty )

    val locationVar : Var[Tab] = Var(Tab.profile)

    val loggedInLocationSignal = locationVar.signal.combineWithFn(loginLevelSignal): ( loc, level ) =>
      level match
        case LoginLevel.high | LoginLevel.low => Some(loc)
        case _ => None

    val loginObserver = Observer[LoginLevel]: level =>
      println(s"loginObserver - level: ${level}")
      if level.isLoggedIn then
        util.sttp.setOptionalVarFromApiGetResult[PosterNoAuth]( protopostLocation.addPath("protopost", "poster-info"), backend, posterNoAuthVar )
        util.sttp.setVarFromTransformedApiGetResult[immutable.Set[DestinationNickname],immutable.SortedSet[DestinationNickname]](
          protopostLocation.addPath("protopost", "destinations"),
          backend,
          destinationsVar,
          immutable.SortedSet.from
        )
      else
        posterNoAuthVar.set(None)
        destinationsVar.set( immutable.SortedSet.empty )

    val loginForm = LoginForm.create( protopostLocation, backend, loginStatusVar, loginLevelSignal, loginLevelChangeEvents )

    val destinationsAndPostsCard = DestinationsAndPostsCard.create(destinationsVar)
    val profileCard = ProfileCard.create(posterNoAuthVar)

    val logoutSubmitter = Observer[dom.MouseEvent]: tup =>
      val transformation : LoginStatus => Option[(LoginStatus,Long)] = loginStatus => Some(Tuple2(loginStatus, epochSecondsNow()))
      val request = basicRequest.post( protopostLocation.addPath("protopost","logout") ).response(asJson[LoginStatus])
      util.sttp.setVarFromTransformedApiResult( request, backend, loginStatusVar, transformation )

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
        try
          println("hard login status request...")
          protopost.client.util.sttp.hardUpdateLoginStatus(protopostLocation, backend, loginStatusVar)
        catch
          case NonFatal(t) =>
            println("hard login status update request failed...");
            t.printStackTrace()
            loginStatusVar.set(None)

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
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        maintainLoginStatus()
        loginLevelChangeEvents.addObserver(loginObserver)
      },
      onUnmountCallback { _ => retireLoginStatus() },
      idAttr("top"),
      // very annoyingly, there's not an easy way to set grid- and hover-related style elements (beyond display.grid itself) in laminar
      styleTag(
        """
        |#app-panel {
        |  /* Use flexbox instead of grid for better Safari compatibility */
        |  display: flex;
        |  flex-direction: column;
        |  height: 100%;
        |}
        |#app-top-panel {
        |  /* Fixed height top section */
        |  flex: 0 0 auto;
        |}
        |#app-card-panel {
        |  /* Flexible middle section that takes remaining space */
        |  flex: 1 1 auto;
        |  overflow: auto;
        |}
        |#app-tab-panel {
        |  /* Fixed height bottom section */
        |  flex: 0 0 auto;
        |}
        |#logout-pane {
        |  color: blue;
        |  transition: background-color 0.3s;
        |}
        |#logout-pane:hover {
        |  color: red;
        |}
        |.tab-pane {
        |  color: blue;
        |  transition: background-color 0.3s;
        |}
        |.tab-pane:hover {
        |  color: green;
        |}
        |.tab-pane.current {
        |  color: black;
        |  font-weight: bold;
        |}
        |.tab-pane.current:hover {
        |  color: black;
        |}
        """.stripMargin
      ),
      div(
        idAttr("inner-top"),
        borderColor <-- loginLevelSignal.map( _.cssColor ),
        borderStyle.solid,
        borderWidth.px(3),
        margin.px(4),
        loginForm.amend(
          display <-- loggedInLocationSignal.map( _.fold("flex")(_ => "none") )
        ),
        div(
          idAttr("app-panel"),
          width.percent(100),
          height.percent(100), // This is fine since parent now has explicit height
          display <-- loggedInLocationSignal.map {
              case Some(_) => "flex"
              case None => "none"
            },
          div(
            idAttr("app-top-panel"),
            display.flex,
            flexDirection.rowReverse,
            //backgroundColor.black,
            paddingTop.px(4),
            paddingRight.px(4),
            fontSize.pt(TinyLinkFontSize),
            a(
              idAttr("logout-pane"),
              "logout",
              onClick --> logoutSubmitter, //{ event => dom.window.alert("logout") },
              cursor("default"),
            )
          ),
          div(
            // card panel
            idAttr("app-card-panel"),
            width.percent(100),
            height.percent(100),
            //marginTop.auto,
            marginBottom.auto,
            destinationsAndPostsCard.amend(
              display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.destinationsAndPosts) then "block" else "none" ),
            ),
            profileCard.amend(
              display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.profile) then "block" else "none" )
            ),
          ),
          div(
            // tab panel
            idAttr("app-tab-panel"),
            display.flex,
            flexDirection.row,
            justifyContent.spaceAround,
            Tab.values.map( t => createTab(t, locationVar) ),
            marginBottom.rem(0.25),

            // why don't these seem to have any effect?
            borderWidth.px(5),
            borderTopWidth.px(5),
            borderBottomWidth.px(5),
            borderLeftWidth.px(5),
            borderRightWidth.px(5),
            borderColor.black,
          )
        )
      )
    )

  def createTab( tab : Tab, locationVar : Var[Tab] ) : HtmlElement =
    div(
      cls := "tab-pane",
      cls <-- locationVar.signal.map( t => if t == tab then "current" else "" ),
      textAlign.center,
      a(
        fontSize.pt(TinyLinkFontSize),
        onClick.map(_ => tab) --> locationVar,
        cursor.default,
        tab.label
      ),
    )
