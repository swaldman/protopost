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

import protopost.common.api.{DestinationIdentifier, Destination, LoginStatus, PostDefinition, PostIdentifier, PosterNoAuth, given}
import protopost.client.util.epochSecondsNow

import scala.util.control.NonFatal
import com.raquo.laminar.nodes.ReactiveHtmlElement

import protopost.common.api.NewPostRevision

object TopPanel:
  private final val LoginStatusUpdateIntervalMsecs         = 6000
  private final val LoginStatusUpdateHardUpdateProbability = 1d/600 // so we hard update about once and hour
  private final val LoginStatusUpdateIfNotUpdatedLastSecs  = 60

  private final val AutosaveCheckFrequencyMsecs = 180000 // autosave every three minutes while actively writing

  private final val TopPanelMargin = 2 //2px

  object Tab:
    import com.github.plokhotnyuk.jsoniter_scala.core.* 
    import com.github.plokhotnyuk.jsoniter_scala.macros.*
    given JsonValueCodec[Tab]= JsonCodecMaker.make
  enum Tab( val label : String ):
    //case newPost  extends Tab("new post")
    case destinationsAndPosts extends Tab("destinations and posts")
    case currentPost          extends Tab("current post")
    case profile              extends Tab("profile")

  def create(protopostLocation : Uri) : HtmlElement =
    val backend : WebSocketBackend[scala.concurrent.Future] = FetchBackend()

    // ( LoginStatus, time last updated in epoch seconds )
    val loginStatusVar : Var[Option[(LoginStatus, Long)]] = Var(None)
    var loginStatusHandle : Option[SetIntervalHandle] = None

    val loginLevelSignal : Signal[LoginLevel] = loginStatusVar.signal.map( _.fold(LoginLevel.unknown)( (ls,_) => LoginLevel.fromLoginStatus(ls) ) )
    val loginLevelChangeEvents = loginLevelSignal.changes.distinct

    val loginFormPrerequisites = LoginForm.Prerequisites(loginStatusVar,loginLevelSignal,loginLevelChangeEvents)

    val posterNoAuthVar : Var[Option[PosterNoAuth]] = Var(None)
    val posterNoAuthSignal : Signal[Option[PosterNoAuth]] = posterNoAuthVar.signal
    val destinationsVar : Var[immutable.SortedSet[Destination]] = Var( immutable.SortedSet.empty )
    val destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]] = Var(Map.empty)

    val currentPostIdentifierLsi = LocalStorageItem(LocalStorageItem.Key.currentPostIdentifier)
    val currentPostIdentifierSignal = currentPostIdentifierLsi.signal

    val topPanelLocationLsi = LocalStorageItem(LocalStorageItem.Key.topPanelLocation)
    val topPanelLocationSignal : Signal[Tab] = topPanelLocationLsi.signal

    val composerLsi = LocalStorageItem(LocalStorageItem.Key.composer)
    val composerSignal = composerLsi.signal

    val currentPostLocalPostContentLsi = LocalStorageItem(LocalStorageItem.Key.currentPostLocalPostContent)
    val currentPostLocalPostContentSignal = currentPostLocalPostContentLsi.signal

    val recoveredRevisionsLsi = LocalStorageItem(LocalStorageItem.Key.recoveredRevisions)

    val localContentDirtyVar : Var[Boolean] = Var(false)

    val openDestinationsLsi = LocalStorageItem(LocalStorageItem.Key.openDestinations)

    val loggedInLocationSignal = topPanelLocationSignal.combineWithFn(loginLevelSignal): ( loc, level ) =>
      level match
        case LoginLevel.high | LoginLevel.low => Some(loc)
        case _ => None

    val currentPostDefinitionSignal = Signal.combine(currentPostIdentifierSignal,destinationsToKnownPostsVar).map: (mbpi,d2kp) =>
      mbpi.flatMap: pi =>
        val mbDestinationMap = d2kp.get(pi.destinationIdentifier)
        mbDestinationMap.flatMap( dm => dm.get(pi.postId) )

    val currentPostDefinitionChanges =
      currentPostDefinitionSignal.changes.distinct.scanLeft[Tuple2[Option[PostDefinition],Option[PostDefinition]]](Tuple2(None,None)): (priorTup,newVal) =>
        (priorTup(1),newVal)

    val disabledTabsSignal = currentPostIdentifierSignal.map: mbPi =>
      mbPi match
        case Some(pi) => Set.empty[Tab]
        case None => Set(Tab.currentPost)

    val manualSaveEventBus : EventBus[Unit] = new EventBus[Unit]
    val manualSaveWriteBus : WriteBus[Unit] = manualSaveEventBus.writer

    val autosaveRequestStream = EventStream.periodic(AutosaveCheckFrequencyMsecs,false)

    val doSaveEventStream : EventStream[NewPostRevision] =
      EventStream.merge(autosaveRequestStream,manualSaveEventBus.events)
        .withCurrentValueOf( localContentDirtyVar, currentPostIdentifierSignal, currentPostLocalPostContentSignal )
        .collect { case (_,true,Some(pd),pc) => NewPostRevision(pd.postId,pc.contentType,pc.text) }
        .distinct

    val loginObserver = Observer[LoginLevel]: level =>
      println(s"loginObserver - level: ${level}")
      if level.isLoggedIn then
        util.request.setOptionalVarFromApiGetResult[PosterNoAuth]( protopostLocation.addPath("protopost", "poster-info"), backend, posterNoAuthVar )
        util.request.setVarFromTransformedApiGetResult[immutable.Set[Destination],immutable.SortedSet[Destination]](
          protopostLocation.addPath("protopost", "destinations"),
          backend,
          destinationsVar,
          immutable.SortedSet.from
        )
      else
        posterNoAuthVar.set(None)
        destinationsVar.set( immutable.SortedSet.empty )

    val currentPostDefinitionChangesObserver = Observer[Tuple2[Option[PostDefinition],Option[PostDefinition]]]: (prev, latest) =>
      util.request.saveLoadOnCurrentPostSwap(protopostLocation,prev,latest,backend,currentPostLocalPostContentLsi,recoveredRevisionsLsi,localContentDirtyVar)

    //val tabModifiers : Map[Tab,Seq[Modifier[HtmlElement]]] =
    //  Map(
    //    Tab.currentPost -> Seq( disabled <-- currentPostIdentifierVar.signal.map( _.isEmpty ) )
    //  )

    val loginForm = LoginForm.create( protopostLocation, backend, loginFormPrerequisites )

    val destinationsAndPostsCard = DestinationsAndPostsCard.create(protopostLocation,backend,currentPostIdentifierLsi,destinationsVar,destinationsToKnownPostsVar,topPanelLocationLsi,posterNoAuthSignal, openDestinationsLsi)
    val currentPostCard =
      CurrentPostCard.create(
        protopostLocation,
        backend,
        destinationsToKnownPostsVar,
        currentPostIdentifierLsi,
        currentPostDefinitionSignal,
        currentPostLocalPostContentLsi,
        localContentDirtyVar,
        posterNoAuthSignal,
        manualSaveWriteBus,
        loginFormPrerequisites
      )
    val profileCard = ProfileCard.create(composerLsi,composerSignal,posterNoAuthSignal)

    val logoutSubmitter = Observer[dom.MouseEvent]: tup =>
      val transformation : LoginStatus => Option[(LoginStatus,Long)] = loginStatus => Some(Tuple2(loginStatus, epochSecondsNow()))
      val request = basicRequest.post( protopostLocation.addPath("protopost","logout") ).response(asJson[LoginStatus])
      util.request.setVarFromTransformedApiResult( request, backend, loginStatusVar, transformation )

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
            val r : Double = math.random
            //println( s"newStatus, lastUpdated: ${newStatus}, ${lastUpdated}" )
            hardUpdate = elapsedSeconds > LoginStatusUpdateIfNotUpdatedLastSecs || r < LoginStatusUpdateHardUpdateProbability
            if hardUpdate then
              println(s"hardUpdate: $elapsedSeconds > ${LoginStatusUpdateIfNotUpdatedLastSecs} || $r < ${LoginStatusUpdateHardUpdateProbability}")
            Some(Tuple2(newStatus, now))
          case None =>
            hardUpdate = true
            None
      if hardUpdate then
        try
          println("hard login status request...")
          protopost.client.util.request.hardUpdateLoginStatus(protopostLocation, backend, loginStatusVar)
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

    def createTab( tab : Tab ) : HtmlElement =
      div(
        cls := "tab-pane",
        idAttr := "tab-pane-${tab}",
        cls <-- topPanelLocationSignal.map( t => if t == tab then "current" else "" ),
        cls <-- disabledTabsSignal.map( tabs => if tabs(tab) then "disabled" else ""),
        textAlign.center,
        TinyLink.create(tab.label).amend(
          onClick( _.withCurrentValueOf(disabledTabsSignal).filter((_,disabled) => !disabled(tab)).map(_ => tab)) --> ( t => topPanelLocationLsi.set(t) ),
        ),
      )

    div(
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        maintainLoginStatus()
        loginLevelChangeEvents.addObserver(loginObserver)
        currentPostDefinitionChanges.addObserver(currentPostDefinitionChangesObserver)
        doSaveEventStream.addObserver( Observer[NewPostRevision]( npr => util.request.saveRevisionToServer(protopostLocation, npr, backend, localContentDirtyVar) ) )
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
        |  max-height: 100%;
        |}
        |#app-top-panel {
        |  /* Fixed height top section */
        |  flex: 0 0 auto;
        |}
        |#app-card-panel {
        |  /* Flexible middle section that takes remaining space */
        |  flex: 1 1 auto;
        |}
        |#app-tab-panel {
        |  /* Fixed height bottom section */
        |  flex: 0 0 auto;
        |}
        |a.click-link {
        |  color: blue;
        |}
        |a.tiny-link {
        |  color: blue;
        |}
        |a.tiny-link:hover {
        |  color: red;
        |}
        |.tab-pane {
        |  color: blue;
        |}
        |.tab-pane a.tiny-link:hover {
        |  color: green;
        |}
        |.tab-pane.current a.tiny-link {
        |  color: black;
        |  font-weight: bold;
        |}
        |.tab-pane.disabled a.tiny-link:hover {
        |  color: gray;
        |}
        |.tab-pane.disabled a.tiny-link {
        |  color: gray;
        |}
        |
        | /* modified from https://getcssscan.com/css-buttons-examples button-23 */
        |.button-utilitarian {
        |  background-color: #FFFFFF;
        |  border: 1px solid #222222;
        |  border-radius: 8px;
        |  box-sizing: border-box;
        |  color: #222222;
        |  cursor: pointer;
        |  display: inline-block;
        |  font-family: sans-serif;
        |  font-size: 11px;
        |  font-weight: bold;
        |  line-height: 13px;
        |  margin: 0;
        |  outline: none;
        |  padding: 0.25em 0.5em;
        |  position: relative;
        |  text-align: center;
        |  text-decoration: none;
        |  touch-action: manipulation;
        |  transition: box-shadow .2s,-ms-transform .1s,-webkit-transform .1s,transform .1s;
        |  user-select: none;
        |  -webkit-user-select: none;
        |  width: auto;
        |}
        |
        |.button-utilitarian:focus-visible {
        |  box-shadow: #222222 0 0 0 2px, rgba(255, 255, 255, 0.8) 0 0 0 4px;
        |  transition: box-shadow .2s;
        |}
        |
        |.button-utilitarian:enabled:active {
        |  background-color: #F7F7F7;
        |  border-color: #000000;
        |  transform: scale(.96);
        |}
        |
        |.button-utilitarian:disabled {
        |  border-color: #DDDDDD;
        |  color: #DDDDDD;
        |  cursor: not-allowed;
        |  opacity: 1;
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
            TinyLink.create("logout").amend(
              idAttr("logout-link"),
              onClick --> logoutSubmitter, //{ event => dom.window.alert("logout") },
            )
          ),
          div(
            // card panel
            idAttr("app-card-panel"),
            //width.percent(100),
            //height.percent(100),
            flexGrow(1),
            overflowX := "clip",
            //marginTop.auto,
            //marginBottom.auto,
            display.flex,
            destinationsAndPostsCard.amend(
              display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.destinationsAndPosts) then "block" else "none" ),
              flexGrow(1),
            ),
            currentPostCard.amend(
              display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.currentPost) then "flex" else "none" ),
              flexGrow(1),
            ),
            profileCard.amend(
              display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.profile) then "block" else "none" ),
              flexGrow(1),
            ),
          ),
          div(
            // tab panel
            idAttr("app-tab-panel"),
            display.flex,
            flexDirection.row,
            justifyContent.spaceAround,
            Tab.values.map( t => createTab(t) ),
            marginTop.rem(0.25),
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

