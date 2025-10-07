package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.collection.immutable
import scala.util.{Success,Failure}
import scala.util.control.NonFatal

import protopost.common.api.{*,given}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object Client:
  val CardPaddingLeftRightRem = 0.5d
  val CardTitleFontSizePt = 18
  val CardSectionTitleFontSizePt = 14
  val CardBaseTextSizePt = 12

  val UntitledPostLabel = "(untitled post)"

  val DefaultComposer = Composer.`text-and-preview`

  val LoginStatusUpdateIntervalMsecs         = 6000
  val LoginStatusUpdateHardUpdateProbability = 1d/600 // so we hard update about once and hour
  val LoginStatusUpdateIfNotUpdatedLastSecs  = 60

  val AutosaveCheckFrequencyMsecs = 180000 // autosave every three minutes while actively writing

  @main
  def main() : Unit =

    // if the query string includes resetLocalStorage=true, reset the local storage and redirect
    val queryString = dom.window.location.search
    val params = new dom.URLSearchParams(queryString)
    if params.has("resetLocalStorage") then
      val rls = Option(params.get("resetLocalStorage")).map( java.lang.Boolean.parseBoolean )
      if rls == Some(true) then
        LocalStorageItem.resetAll()
        // Remove resetLocalStorage param and redirect to prevent accidental re-clearing on reload
        params.delete("resetLocalStorage")
        val newSearch = params.toString()
        val newUrl = dom.window.location.pathname + (if newSearch.isEmpty then "" else "?" + newSearch)
        dom.window.location.href = newUrl
        return

    lazy val container = dom.document.getElementById("root")
    val client = new Client(uri"${Globals.protopostLocation}" )
    render( container, client.element() )

class Client( val protopostLocation : Uri ):
  import Client.*

  val backend : WebSocketBackend[scala.concurrent.Future] = FetchBackend()

  // ( LoginStatus, time last updated in epoch seconds, should hard update to recheck )
  val loginStatusVar : Var[Option[(LoginStatus, Long, Boolean)]] = Var(None)
  val loginStatusSignal = loginStatusVar.signal

  val loginLevelSignal : Signal[LoginLevel] = loginStatusVar.signal.map( _.fold(LoginLevel.unknown)( (ls,_,_) => LoginLevel.fromLoginStatus(ls) ) )
  val loginLevelChangeEvents = loginLevelSignal.changes.distinct

  val posterNoAuthVar : Var[Option[PosterNoAuth]] = Var(None)
  val posterNoAuthSignal : Signal[Option[PosterNoAuth]] = posterNoAuthVar.signal
  val destinationsVar : Var[immutable.SortedSet[Destination]] = Var( immutable.SortedSet.empty )
  val destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]] = Var(Map.empty)

  val currentPostIdentifierLsi = LocalStorageItem(LocalStorageItem.Key.currentPostIdentifier)
  val currentPostIdentifierSignal = currentPostIdentifierLsi.signal

  val topPanelLocationLsi = LocalStorageItem(LocalStorageItem.Key.topPanelLocation)
  val topPanelLocationSignal : Signal[TopPanel.Tab] = topPanelLocationLsi.signal

  val composerLsi = LocalStorageItem(LocalStorageItem.Key.composer)
  val composerSignal = composerLsi.signal

  val currentPostLocalPostContentLsi = LocalStorageItem(LocalStorageItem.Key.currentPostLocalPostContent)
  val currentPostLocalPostContentSignal = currentPostLocalPostContentLsi.signal

  val recoveredRevisionsLsi = LocalStorageItem(LocalStorageItem.Key.recoveredRevisions)

  val localContentDirtyVar : Var[Boolean] = Var(false)

  val openDestinationsLsi = LocalStorageItem(LocalStorageItem.Key.openDestinations)

  val currentPostDefinitionSignal = Signal.combine(currentPostIdentifierSignal,destinationsToKnownPostsVar).map: (mbpi,d2kp) =>
    mbpi.flatMap: pi =>
      val mbDestinationMap = d2kp.get(pi.destinationIdentifier)
      mbDestinationMap.flatMap( dm => dm.get(pi.postId) )

  val currentPostDefinitionChangeEvents = currentPostDefinitionSignal.changes.distinct

  val currentPostDefinitionLastChangeTuple =
    currentPostDefinitionChangeEvents.scanLeft[Tuple2[Option[PostDefinition],Option[PostDefinition]]](Tuple2(None,None)): (priorTup,newVal) =>
      (priorTup(1),newVal)

  val currentPostAllRevisionsVar : Var[Option[PostRevisionHistory]] = Var(None)

  private val manualSaveEventBus : EventBus[Unit] = new EventBus[Unit]

  val manualSaveWriteBus : WriteBus[Unit] = manualSaveEventBus.writer

  private val updateLoginStatusStream = EventStream.periodic(LoginStatusUpdateIntervalMsecs,false)

  private val autosaveRequestStream = EventStream.periodic(AutosaveCheckFrequencyMsecs,false)

  private val doSaveEventStream : EventStream[NewPostRevision] =
    EventStream.merge(autosaveRequestStream,manualSaveEventBus.events)
      .withCurrentValueOf( localContentDirtyVar, currentPostIdentifierSignal, currentPostLocalPostContentSignal )
      .collect { case (_,true,Some(pd),pc) => NewPostRevision(pd.postId,pc.contentType,pc.text) }
      .distinct

  private val loginObserver = Observer[LoginLevel]: level =>
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


  private val updateLoginStatusObserver = Observer[Int]: count =>
    // on initial mount, the update seems sometimes to skip,
    // perhaps a race condition as mount is not completed?
    //
    // so we default to a hard update if update doesn't work out.
    loginStatusVar.update: optTup =>
      //println("updateLoginStatus() -- optTup: " + optTup);
      optTup match
        case Some(Tuple3(ls,lastUpdated,lastHardUpdate)) =>
          val now = util.epochSecondsNow()
          val elapsedSeconds = now - lastUpdated
          val newStatus = LoginStatus( math.max(0,ls.highSecuritySecondsRemaining - elapsedSeconds), math.max(0,ls.lowSecuritySecondsRemaining - elapsedSeconds) )
          val r : Double = math.random
          //println( s"newStatus, lastUpdated: ${newStatus}, ${lastUpdated}" )
          val hardUpdate = lastHardUpdate || elapsedSeconds > LoginStatusUpdateIfNotUpdatedLastSecs || r < LoginStatusUpdateHardUpdateProbability
          //println(s"hardUpdate: $hardUpdate")
          if hardUpdate then
            println(s"hardUpdate: $elapsedSeconds > ${LoginStatusUpdateIfNotUpdatedLastSecs} || $r < ${LoginStatusUpdateHardUpdateProbability}")
          Some(Tuple3(newStatus, now, hardUpdate))
        case None =>
          None

  private val mustHardUpdateLoginStatusObserver = Observer[Option[(LoginStatus,Long,Boolean)]]: mbls =>
    def doHardUpdate() =
      try
        println("hard login status request...")
        protopost.client.util.request.hardUpdateLoginStatus(protopostLocation, backend, loginStatusVar)
      catch
        case NonFatal(t) =>
          println("hard login status update request failed...");
          t.printStackTrace()
          loginStatusVar.set(None)
    mbls match
      case Some(Tuple3(_,_,hu)) if hu => doHardUpdate()
      case Some( _ )                  => /* ignore */
      case None                       => doHardUpdate()

  private val currentPostDefinitionLastChangeTupleObserver = Observer[Tuple2[Option[PostDefinition],Option[PostDefinition]]]: (prev, latest) =>
    util.request.saveLoadOnCurrentPostSwap(protopostLocation,prev,latest,backend,currentPostLocalPostContentLsi,recoveredRevisionsLsi,localContentDirtyVar)

  def element() : HtmlElement =
    div(
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        updateLoginStatusStream.addObserver( updateLoginStatusObserver )
        loginStatusSignal.addObserver( mustHardUpdateLoginStatusObserver )
        loginLevelChangeEvents.addObserver(loginObserver)
        currentPostDefinitionLastChangeTuple.addObserver(currentPostDefinitionLastChangeTupleObserver)
        doSaveEventStream.addObserver( Observer[NewPostRevision]( npr => util.request.saveRevisionToServer(protopostLocation, npr, backend, localContentDirtyVar) ) )
      },
      idAttr("protopost-client-default"),
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
      TopPanel.create( this )
    )
