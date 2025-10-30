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

import scala.scalajs.js
import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import java.time.format.DateTimeFormatter
import java.time.ZoneId

import protopost.common.PosterId
import protopost.common.api.{*,given}

import protopost.client.util.rebasePage
import protopost.client.util.laminar.VarLike

object Client:
  val CardPaddingLeftRightRem = 0.5d
  val CardTitleFontSizePt = 18
  val CardSectionTitleFontSizePt = 14
  val CardBaseTextSizePt = 12

  val UntitledPostLabel = "(untitled post)"

  val DefaultComposer = Composer.WYSIWYG

  val LoginStatusUpdateIntervalMsecs         = 6000
  val LoginStatusUpdateHardUpdateProbability = 1d/600 // so we hard update about once and hour
  val LoginStatusUpdateIfNotUpdatedLastSecs  = 300

  val UnsuccessfulLoginStatusCheckRetryMs = 60000 // try again in a minute if we failed to login

  val AutosaveCheckFrequencyMsecs = 180000 // autosave every three minutes while actively writing

  val PublishDetailsPaneLabelCommonModifiers = Seq( fontSize.pt(11), fontWeight.bold )

  val RevisionTimestampFormatter = DateTimeFormatter.ofPattern("""yyyy'-'MM'-'dd' @ 'hh':'mm' 'a""").withZone( ZoneId.systemDefault() )

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
  val destinationsSignal = destinationsVar.signal

  val destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]] = Var(Map.empty)
  val destinationsToKnownPostsSignal = destinationsToKnownPostsVar.signal

  object currentPostIdentifierManager extends util.laminar.VarLike[Option[PostIdentifier]]:
    private def rebase( mbpi : Option[PostIdentifier] ) : Unit =
      rebasePage( mbpi.map( pi => protopostLocation.addPath("protopost","post-media",pi.postId.toString,"").toString ) ) // we add the empty string to get the directory trailing slash

    private def setGlobal( mbpi : Option[PostIdentifier] ) : Unit =
      mbpi match
        case Some( pi ) => Globals.protopostCurrentPostId = pi.postId
        case None => Globals.protopostCurrentPostId = 0 // falsey!

    private def init() : Unit =
      val start = lsi.now()
      rebase(start)
      setGlobal(start)

    val lsi = LocalStorageItem(LocalStorageItem.Key.currentPostIdentifier)

    init()

    def set( mbpi : Option[PostIdentifier] ) =
      rebase( mbpi )
      setGlobal( mbpi )
      lsi.set(mbpi)

    def update( doUpdate : Option[PostIdentifier] => Option[PostIdentifier] ) =
      val updated = doUpdate( lsi.now() )
      set( updated )

    def signal : Signal[Option[PostIdentifier]] = lsi.signal
  end currentPostIdentifierManager

  val currentPostIdentifierSignal = currentPostIdentifierManager.signal

  val topPanelLocationLsi = LocalStorageItem(LocalStorageItem.Key.topPanelLocation)
  val topPanelLocationSignal : Signal[TopPanel.Tab] = topPanelLocationLsi.signal

  val composerLsi = LocalStorageItem(LocalStorageItem.Key.composer)
  val composerSignal = composerLsi.signal

  val newPostContentTypeSignal = composerSignal.map: composer =>
    composer match
      case Composer.WYSIWYG => "text/html"
      case Composer.`text-and-preview` => "text/markdown"

  val currentPostLocalPostContentLsi = LocalStorageItem(LocalStorageItem.Key.currentPostLocalPostContent)
  val currentPostLocalPostContentSignal = currentPostLocalPostContentLsi.signal

  val recoveredRevisionsLsi = LocalStorageItem(LocalStorageItem.Key.recoveredRevisions)
  val recoveredRevisionsSignal = recoveredRevisionsLsi.signal

  val localContentDirtyVar : Var[Boolean] = Var(false)
  val localContentDirtySignal = localContentDirtyVar.signal

  val openDestinationsLsi = LocalStorageItem(LocalStorageItem.Key.openDestinations)

  val lastLoggedInPosterLsi = LocalStorageItem(LocalStorageItem.Key.lastLoggedInPoster)
  val lastLoggedInPosterSignal = lastLoggedInPosterLsi.signal

  val currentPostDefinitionSignal = Signal.combine(currentPostIdentifierSignal,destinationsToKnownPostsVar).map: (mbpi,d2kp) =>
    mbpi.flatMap: pi =>
      val mbDestinationMap = d2kp.get(pi.destinationIdentifier)
      mbDestinationMap.flatMap( dm => dm.get(pi.postId) )

  val currentPostDefinitionChangeEvents = currentPostDefinitionSignal.changes.distinct

  val currentPostDefinitionLastChangeTuple =
    currentPostDefinitionChangeEvents.scanLeft[Tuple2[Option[PostDefinition],Option[PostDefinition]]](Tuple2(None,None)): (priorTup,newVal) =>
      (priorTup(1),newVal)

  val composerPaneCurrentTabVar : Var[ComposerPane.Tab] = Var(ComposerPane.Tab.edit)
  val composerPaneCurrentTabSignal = composerPaneCurrentTabVar.signal
  val ckEditorComposerPaneCurrentTabVar : Var[CkEditorComposerPane.Tab] = Var(CkEditorComposerPane.Tab.edit)
  val ckEditorComposerPaneCurrentTabSignal = ckEditorComposerPaneCurrentTabVar.signal

  def resetComposersToEdit() : Unit =
    composerPaneCurrentTabVar.set(ComposerPane.Tab.edit)
    ckEditorComposerPaneCurrentTabVar.set(CkEditorComposerPane.Tab.edit)

  def localContentStatusCircle() : HtmlElement =
    util.laminar.statusCircle().amend(
      backgroundColor <-- localContentDirtySignal.map( dirty => if dirty then "yellow" else "#22ff22" )
    )

  val currentPostAllRevisionsVar : Var[Option[PostRevisionHistory]] = Var(None)
  val currentPostAllRevisionsSignal = currentPostAllRevisionsVar.signal

  val currentPostMediaVar : Var[Option[Seq[PostMediaInfo]]] = Var(None)
  val currentPostMediaSignal = currentPostMediaVar.signal

  val selectedUnsavedRevisionVar : Var[Option[UnsavedRevision]] = Var(None)
  val selectedUnsavedRevisionSignal = selectedUnsavedRevisionVar.signal

  object externalJsConfigManager extends VarLike[ProtopostExternalJsConfig]:
    private def init() : Unit =
      val externalJsConfig = lsi.now().toJsObject

      /*
      // see https://www.scala-js.org/news/2020/02/25/announcing-scalajs-1.0.0/
      // "The js.typeOf “method” is magical when its argument is a member of a global scope object."

      if js.typeOf(js.Dynamic.global.protopostExternalJsConfig) == "undefined" then
          js.Dynamic.global.updateDynamic("protopostExternalJsConfig")(externalJsConfig) // oops... fails with ReferenceError
      else
        Globals.protopostExternalJsConfig = externalJsConfig
      */

      // we'll just make sure (empty) protopostExternalJsConfig already exists, in our launch javascript
      Globals.protopostExternalJsConfig = externalJsConfig

    end init

    val lsi = LocalStorageItem(LocalStorageItem.Key.externalJsConfig)

    init()

    def set( pejc : ProtopostExternalJsConfig ) =
      Globals.protopostExternalJsConfig = pejc.toJsObject
      lsi.set(pejc)

    def update( doUpdate : ProtopostExternalJsConfig => ProtopostExternalJsConfig ) =
      val updated = doUpdate( lsi.now() )
      set( updated )

    def signal : Signal[ProtopostExternalJsConfig] = lsi.signal
  end externalJsConfigManager

  val externalJsConfigSignal = externalJsConfigManager.signal

  private val manualSaveEventBus : EventBus[Unit] = new EventBus[Unit]

  val manualSaveWriteBus : WriteBus[Unit] = manualSaveEventBus.writer

  val serifFontFamilies = "Georgia, Garamond, serif"

  val sectionBorderPaddingMargin = Seq(
    marginTop.rem(0.5),
    padding.rem(1),
    borderStyle.solid,
    borderColor.black,
    borderWidth.px(2),
    borderRadius.px(10),
  )

  val composerRawTextAreaChangeObserver = Observer[String]: (value) =>
    currentPostLocalPostContentLsi.update( _.copy(text=value) )
    localContentDirtyVar.set(true)

  private val updateLoginStatusStream = EventStream.periodic(LoginStatusUpdateIntervalMsecs,false)

  private val autosaveRequestStream = EventStream.periodic(AutosaveCheckFrequencyMsecs,false)

  private val doSaveEventStream : EventStream[NewPostRevision] =
    EventStream.merge(autosaveRequestStream,manualSaveEventBus.events)
      .withCurrentValueOf( localContentDirtyVar, currentPostIdentifierSignal, currentPostLocalPostContentSignal )
      .collect { case (_,true,Some(pd),pc) => NewPostRevision(pd.postId,pc.contentType,pc.text) }
      //.distinct // if it's distinct, edits that make a change then undo it can't clear the dirty var, because the save is not even attempted

  private val reloadPostMediaRequestEventBus = new EventBus[Unit]
  private val reloadPostMediaRequestWriteBus = reloadPostMediaRequestEventBus.writer
  private val reloadPostMediaRequestListener : js.Function1[dom.Event, Unit] = (evt: dom.Event) => reloadPostMediaRequestWriteBus.onNext( () )
  private val reloadPostMediaRequestObserver = Observer[Option[PostDefinition]]: mbpd =>
    mbpd match
      case Some( pd ) =>
        util.request.loadCurrentPostMedia(protopostLocation,pd.postId,backend,currentPostMediaVar)
      case None =>
        dom.console.warn("We're seeing a 'ckeditorUploadComplete' event while there is no current post to update for?")


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

  private val localStorageUserFreshnessObserver = Observer[(Option[PosterNoAuth],Option[PosterId])]: ( mbPna, mbPid ) =>
    ( mbPna, mbPid ) match
      case ( Some(pna), Some(posterId) ) => // we're logging in, we have local storage set for current user, ensure consistency
        if pna.id != posterId then // inconsistent user, clear local storage
          LocalStorageItem.resetAll()
      case ( Some(pna), None ) =>  // we're logging in, we have no local storage set for current user, set it fresh
        LocalStorageItem.resetAll()
        lastLoggedInPosterLsi.set(Some(pna.id))
      case ( None, Some(posterId) ) =>  // we're logging out, we have local storage set, don't clear it, we may re-login consistently
        /* ignore */
      case (None,None) =>
        /* apparently the app has no storage for any user, but to be sure we can clear local storage */
        LocalStorageItem.resetAll()

  private val doSaveEventObserver = Observer[NewPostRevision]: npr =>
    // dom.console.log( "Saving revision.", npr )
    util.request.saveRevisionToServerUpdateRevisions(protopostLocation, npr, backend, localContentDirtyVar, currentPostAllRevisionsVar)

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
            println(s"hardUpdate: $lastHardUpdate || $elapsedSeconds > ${LoginStatusUpdateIfNotUpdatedLastSecs} || $r < ${LoginStatusUpdateHardUpdateProbability}")
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

  private val currentPostDefinitionLastChangeTupleObserver = Observer[Tuple3[Option[PostDefinition],Option[PostDefinition],String]]: (prev, latest, newPostContentType) =>
    util.request.saveLoadOnCurrentPostSwap(protopostLocation,prev,latest,newPostContentType,backend,currentPostLocalPostContentLsi,recoveredRevisionsLsi,localContentDirtyVar)
    latest match
      case Some( pd ) =>
        util.request.loadCurrentPostRevisionHistory(protopostLocation,pd.postId,backend,currentPostAllRevisionsVar)
        util.request.loadCurrentPostMedia(protopostLocation,pd.postId,backend,currentPostMediaVar)
      case None =>
        currentPostAllRevisionsVar.set( None )
        currentPostMediaVar.set( None )

  def updateCurrentPostRevisions( postId : Int ) =
    util.request.loadCurrentPostRevisionHistory(protopostLocation,postId,backend,currentPostAllRevisionsVar)

  def element() : HtmlElement =
    div(
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        updateLoginStatusStream.addObserver( updateLoginStatusObserver )
        loginStatusSignal.changes.throttle(UnsuccessfulLoginStatusCheckRetryMs).addObserver( mustHardUpdateLoginStatusObserver ) // important that these changes are NOT distinct, so we retry every minute, even on the "same" failure
        loginLevelChangeEvents.addObserver(loginObserver)
        currentPostDefinitionLastChangeTuple.withCurrentValueOf(newPostContentTypeSignal).addObserver(currentPostDefinitionLastChangeTupleObserver)
        doSaveEventStream.addObserver( doSaveEventObserver )
        posterNoAuthSignal.withCurrentValueOf(lastLoggedInPosterSignal).addObserver( localStorageUserFreshnessObserver ) // monitor for changes in user logged in to this browser, clear local storage if there is a change
        reloadPostMediaRequestEventBus.events.withCurrentValueOf(currentPostDefinitionSignal).addObserver( reloadPostMediaRequestObserver )

        // we don't want to erase the local-storage current post on app startup, so we don't update the current post based on selectedUnsavedRevision when it is None
        selectedUnsavedRevisionSignal.addObserver( Observer[Option[UnsavedRevision]](mbur => if mbur.nonEmpty then currentPostIdentifierManager.set( mbur.map(_.postIdentifier))) ) 

        dom.document.addEventListener("ckeditorUploadComplete", reloadPostMediaRequestListener)
      },
      onUnmountCallback { elem =>
        dom.document.removeEventListener("ckeditorUploadComplete", reloadPostMediaRequestListener)
      },
      idAttr("protopost-client-default"),
      height.percent(100),
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
        |#profile-recovered-revisions a.tiny-link:hover {
        |  color: green;
        |}
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
        |
        |/* DestinationsAndPostsCard */
        |
        |#destinations-and-posts-panel a:hover {
        |  text-decoration: underline;
        |}
        |
        |/* ComposerPane */
        |
        |#composer-text-area:focus, #ckeditor-composer-source-text-area:focus {
        |  border-color: black;
        |  outline: none;
        |}
        |
        |#revisons-cards-timestamp-list a.tiny-link:hover {
        |  color: black;
        |  background-color: yellow;
        |}
        |
        |.scrollbars-hidden {
        |   scrollbar-width: none;
        |   -ms-overflow-style: none;
        |}
        |.scrollbars-hidden::-webkit-scrollbar {
        |   display: none;
        |}
        """.stripMargin
      ),
      TopPanel.create( this )
    )
