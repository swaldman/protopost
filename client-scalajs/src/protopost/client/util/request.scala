package protopost.client.util.request

import sttp.client4.*
import sttp.model.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import org.scalajs.dom
import com.raquo.laminar.api.L.*
import protopost.client.util.epochSecondsNow
import protopost.client.util.laminar.VarLike
import protopost.client.{Client,LocalStorageItem,LoginLevel,ReverseChronologicalPostDefinitions}
import protopost.common.api.{LoginStatus,given}
import scala.util.{Success,Failure}
import scala.collection.immutable
import scala.concurrent.{ExecutionContext,Future}
import scala.util.control.NonFatal
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import protopost.common.api.*
import protopost.client.PostContent
import java.time.Instant
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.Int8Array
import protopost.client.UnsavedRevision

/*
def rawBodyToLoginLevelOrThrow( rawBody : Either[ResponseException[String], LoginStatus] ) : LoginLevel =
  rawBody match
    case Left( oops ) => throw oops //new Exception( oops.toString() )
    case Right( loginStatus ) => LoginLevel.fromLoginStatus( loginStatus )
*/

private def fragileParseOriginalMessageFromResponseException( re : ResponseException[?] ) : String =
  val raw = re.toString()
  val stop = raw.indexOf("\tat")
  if stop >= 0 then
    //println(s"stop: $stop")
    val line = raw.substring(0,stop)
    //println( s"line: $line" )
    val lastColon = line.lastIndexOf(":")
    //println(s"lastColon: $lastColon")
    if lastColon >= 0 then
      line.substring(lastColon+1).trim
    else
      line
  else
    raw

def unsubscribeDestinationFromFeed(
  protopostLocation : Uri,
  destination : Destination,
  feedId : Int,
  backend : WebSocketBackend[scala.concurrent.Future],
  destinationToFeedsVar : Var[immutable.SortedMap[Destination,immutable.SortedSet[SubscribableFeed]]]
)(using ec : ExecutionContext) : Unit =
  val di = destination.destinationIdentifier
  val request =
    basicRequest.delete(
      protopostLocation.addPath("protopost", "subscribe-to-rss-for-comments", di.seismicNodeId.toString, di.name, feedId.toString )
    )
  val future = request.send(backend)
  future.onComplete: attempt =>
    attempt match
      case Success( response ) =>
        response.body match
          case Right( _ ) =>
            updateFeedsForDestination( protopostLocation, destination, backend, destinationToFeedsVar )
          case Left( errorMessage ) =>
            println("Error while attempting to unsubscribe from feed:")
            println( errorMessage )
      case Failure( t ) =>
        println(s"In network error handler, with Throwable ${t}")
        t.printStackTrace()

def subscribeDestinationToFeedsFromFeedSource(
  protopostLocation : Uri,
  destination : Destination,
  feedSource : String,
  backend : WebSocketBackend[scala.concurrent.Future],
  messageVar : Var[Option[String]],
  destinationToFeedsVar : Var[immutable.SortedMap[Destination,immutable.SortedSet[SubscribableFeed]]]
)(using ec : ExecutionContext) : Unit =
  val di = destination.destinationIdentifier
  val request =
    basicRequest
      .post( protopostLocation.addPath("protopost", "subscribe-to-rss-for-comments" ) )
      .body( asJson( RssSubscriptionRequest( destination.destinationIdentifier, feedSource ) ) )
      .response( asJson[RssSubscriptionResponse] )

  val future = request.send(backend)
  future.onComplete: attempt =>
    attempt match
      case Success( response ) =>
        response.body match
          case Right( _ ) =>
            // we want all the feeds, not just the new subscriptions
            updateFeedsForDestination( protopostLocation, destination, backend, destinationToFeedsVar )
          case Left( responseException ) =>
            // println(s"In error handler, with ResponseException ${responseException}")
            // dom.console.error( s"Response: ${responseException.response}" )
            val message = fragileParseOriginalMessageFromResponseException( responseException )
            println( s"ResponseException, parsed message: $message" )
            println( "ResponseException:" )
            println( responseException )
            //responseException.printStackTrace()
            messageVar.set( Some( s"Error: $message" ) )
      case Failure( t ) =>
        println(s"In network error handler, with Throwable ${t}")
        t.printStackTrace()
        messageVar.set( Some( t.getMessage() ) )

def updateFeedsForDestination(
  protopostLocation : Uri,
  destination : Destination,
  backend : WebSocketBackend[scala.concurrent.Future],
  destinationToFeedsVar : Var[immutable.SortedMap[Destination,immutable.SortedSet[SubscribableFeed]]]
)(using ec : ExecutionContext) : Unit =
  val di = destination.destinationIdentifier
  val request =
    basicRequest
      .get( protopostLocation.addPath("protopost", "rss-subscriptions-by-destination", di.seismicNodeId.toString, di.name) )
      .response( asJson[Set[SubscribableFeed]] )
  val updater : Set[SubscribableFeed] => immutable.SortedMap[Destination,immutable.SortedSet[SubscribableFeed]] => immutable.SortedMap[Destination,immutable.SortedSet[SubscribableFeed]] =
    feeds =>
      map =>
        val sorted = immutable.SortedSet.from(feeds)
        map + Tuple2( destination, sorted )
  updateVarFromApiResult( request, backend, destinationToFeedsVar, updater )

def deleteMediaItemForPost(
  protopostLocation : Uri,
  postId : Int,
  fullPath : String,
  backend : WebSocketBackend[scala.concurrent.Future],
  currentPostMediaVar : Var[Option[Seq[PostMediaInfo]]]
)(using ec : ExecutionContext) : Unit =
  val pathElements =
    fullPath.split("/").toList
  val future =  
    val request = basicRequest.delete( protopostLocation.addPath( ("protopost"::"post-media"::postId.toString()::pathElements) ) )
    request.send(backend)
  future.onComplete: attempt =>
    attempt match
      case Success( pmi ) => loadCurrentPostMedia( protopostLocation, postId, backend, currentPostMediaVar )
      case Failure( t )   => t.printStackTrace()

def writeMediaItemForPost(
  protopostLocation : Uri,
  postId : Int,
  fullPath : String,
  file : dom.File,
  backend : WebSocketBackend[scala.concurrent.Future],
  currentPostMediaVar : Var[Option[Seq[PostMediaInfo]]]
)(using ec : ExecutionContext) : Unit =
  val pathElements =
    fullPath.split("/").toList
  def findData() : Future[Array[Byte]] =
    file.arrayBuffer().toFuture.map( ab => new Int8Array(ab,0,math.round(file.size).toInt).toArray )
  def runRequest( bytes : Array[Byte] ) =
    val request =
      basicRequest
        .post( protopostLocation.addPath( ("protopost"::"upload-post-media"::postId.toString()::pathElements) ) )
        .contentType( file.`type` )
        .body( bytes )
        .response( asJson[PostMediaInfo] )
    request.send(backend).map( _.body ).map( decodeOrThrow )
  val future = findData().flatMap( runRequest )  
  future.onComplete: attempt =>
    attempt match
      case Success( pmi ) => loadCurrentPostMedia( protopostLocation, postId, backend, currentPostMediaVar )
      case Failure( t )   => t.printStackTrace()
  
def saveLoadOnCurrentPostSwap(
  protopostLocation              : Uri,
  mbPrevPostDefinition           : Option[PostDefinition],
  mbNewPostDefinition            : Option[PostDefinition],
  newPostContentType             : String,
  backend                        : WebSocketBackend[scala.concurrent.Future],
  currentPostLocalPostContentLsi : LocalStorageItem[PostContent],
  recoveredRevisionsLsi          : LocalStorageItem[List[UnsavedRevision]],
  localContentDirtyVar           : Var[Boolean]
)(using ec : ExecutionContext) =
  //println(s"saveLoadOnCurrentPostSwap(...): mbPrevPostDefinition: $mbPrevPostDefinition, mbNewPostDefinition: $mbNewPostDefinition")
  def saveCurrent(ppd : PostDefinition) : Future[Unit] =
    val PostContent(contentType, body) = currentPostLocalPostContentLsi.now()
    val npr = NewPostRevision(ppd.postId, contentType, body)
    //println( s"nickname: ${ppd.destination.nickname}" )
    def saveCurrentOrFail() : Future[Unit] =
        val request =
          basicRequest
            .post( protopostLocation.addPath("protopost", "new-draft") )
            .body( asJson(npr) )
            .response( asJson[Option[PostRevisionIdentifier]] )
        request.send(backend).map( _.body ).map( _ => localContentDirtyVar.set(false) )
    def saveAsRecovered() : Unit =
      val rt = RevisionTimestamp( Instant.now() )
      recoveredRevisionsLsi.update( list => UnsavedRevision(rt,npr,ppd.destination) :: list )
    if body.nonEmpty then // don't potentially overwrite latest as strictly empty. more likely a bug than an intention
      saveCurrentOrFail().recover:
        case NonFatal(t) =>
          println("Failed to save revision to server:")
          t.printStackTrace()
          println("Saving revision to recovered revisions.")
          saveAsRecovered()
    else
      Future.unit
  def loadNew(npd : PostDefinition) : Future[Unit] =
    val request =
      basicRequest
        .get( protopostLocation.addPath("protopost", "latest-draft", npd.postId.toString) )
        .response( asJson[Option[RetrievedPostRevision]] )
    request.send(backend).map( _.body ).map( decodeOrThrow ).map: mbPostRevision =>
      mbPostRevision match
        case Some( pr ) => currentPostLocalPostContentLsi.set( PostContent( pr.contentType, pr.body ) )
        case None => currentPostLocalPostContentLsi.set( PostContent.default.copy(contentType = newPostContentType) )
      localContentDirtyVar.set(false)
  (mbPrevPostDefinition,mbNewPostDefinition) match
    case (Some(ppd),Some(npd)) =>
      saveCurrent(ppd).flatMap( _ => loadNew(npd) )
    case (None,Some(npd)) => 
      if currentPostLocalPostContentLsi.now() != PostContent.default then
        // this occurs on app initialization, we want to preserve local post content
        localContentDirtyVar.set(true)
      else
        // there is no local content we want to preserve, let's load from the server
        loadNew(npd)
    case (Some(npd),None) =>
      currentPostLocalPostContentLsi.set( PostContent.default )
    case (None,None) =>
      /* do nothing, we shouldn't see this */
end saveLoadOnCurrentPostSwap

def loadPostRevision(
  protopostLocation : Uri,
  postId : Int,
  revisionTimestamp : RevisionTimestamp,
  backend : WebSocketBackend[scala.concurrent.Future],
  mbRevisionsVar : Var[Option[RetrievedPostRevision]]
)(using ec : ExecutionContext) =
  val request =
    basicRequest
      .get( protopostLocation.addPath("protopost", "retrieve-revision", postId.toString, revisionTimestamp.timestampEpochSeconds.toString, revisionTimestamp.timestampNanos.toString) )
      .response( asJson[RetrievedPostRevision] )
  val updater : RetrievedPostRevision => Option[RetrievedPostRevision] => Option[RetrievedPostRevision] = (rh => (_ => Some(rh)))
  updateVarFromApiResult[RetrievedPostRevision,Option[RetrievedPostRevision]]( request, backend, mbRevisionsVar, updater )

def loadCurrentPostRevisionHistory(
  protopostLocation : Uri,
  postId : Int,
  backend : WebSocketBackend[scala.concurrent.Future],
  currentPostAllRevisionsVar : Var[Option[PostRevisionHistory]]
)(using ec : ExecutionContext) =
  // println("loadCurrentPostRevisionHistory(...)")
  val request =
    basicRequest
      .get( protopostLocation.addPath("protopost", "revision-history", postId.toString) )
      .response( asJson[PostRevisionHistory] )
  val updater : PostRevisionHistory => Option[PostRevisionHistory] => Option[PostRevisionHistory] = (rh => (_ => Some(rh)))
  updateVarFromApiResult[PostRevisionHistory,Option[PostRevisionHistory]]( request, backend, currentPostAllRevisionsVar, updater )

def loadCurrentPostMedia(
  protopostLocation : Uri,
  postId : Int,
  backend : WebSocketBackend[scala.concurrent.Future],
  currentPostMediaVar : Var[Option[Seq[PostMediaInfo]]]
)(using ec : ExecutionContext) =
  // println("loadCurrentPostRevisionHistory(...)")
  val request =
    basicRequest
      .get( protopostLocation.addPath("protopost", "post-media-by-post-id", postId.toString) )
      .response( asJson[Seq[PostMediaInfo]] )
  val updater : Seq[PostMediaInfo] => Option[Seq[PostMediaInfo]] => Option[Seq[PostMediaInfo]] = (spmi => (_ => Some(spmi)))
  updateVarFromApiResult[Seq[PostMediaInfo],Option[Seq[PostMediaInfo]]]( request, backend, currentPostMediaVar, updater )

def rawBodyToLoginStatusOrThrow( rawBody : Either[ResponseException[String], LoginStatus] ) : LoginStatus =
  rawBody match
    case Left( oops ) => throw oops //new Exception( oops.toString() )
    case Right( loginStatus ) => loginStatus

def saveRevisionToServerUpdateRevisions(
  protopostLocation : Uri,
  npr : NewPostRevision,
  backend : WebSocketBackend[scala.concurrent.Future],
  localContentDirtyVar : Var[Boolean],
  currentPostAllRevisionsVar : Var[Option[PostRevisionHistory]]
)(using ec : ExecutionContext) =
  val request =
    basicRequest
      .post( protopostLocation.addPath("protopost", "new-draft") )
      .body( asJson(npr) )
      .response( asJson[Option[PostRevisionIdentifier]] )
  val updater : Option[PostRevisionIdentifier] => Boolean => Boolean = (_ => (_ => false))
  val sideEffectPostUpdate : Option[PostRevisionIdentifier] => Unit =
    (mbpri : Option[PostRevisionIdentifier]) =>
      mbpri match
        case Some( pri ) =>
          loadCurrentPostRevisionHistory(protopostLocation,npr.postId,backend,currentPostAllRevisionsVar)
        case None =>
          /* nothing to do? what does it mean if we got None back?                    */
          /* It means nothing was actually saved because the revision hadn't changed. */
  updateVarFromApiResult[Option[PostRevisionIdentifier],Boolean]( request, backend, localContentDirtyVar, updater, sideEffectPostUpdate )

def hardUpdateNewPostDefinition(
  protopostLocation : Uri,
  destinationIdentifier : DestinationIdentifier,
  postDefinitionCreate : PostDefinitionCreate,
  backend : WebSocketBackend[scala.concurrent.Future],
  destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
  currentPostIdentifierManager : VarLike[Option[PostIdentifier]]
)(using ec : ExecutionContext) =
  val request =
    basicRequest
      .post( protopostLocation.addPath("protopost", "new-post") )
      .body( asJson(postDefinitionCreate) )
      .response( asJson[Option[PostDefinition]] )
  val updater : Option[PostDefinition] => Map[DestinationIdentifier,Map[Int,PostDefinition]] => Map[DestinationIdentifier,Map[Int,PostDefinition]] =
    mbPostDefinition =>
      mbPostDefinition match
        case Some( postDefinition ) =>
          ( map : Map[DestinationIdentifier,Map[Int,PostDefinition]] ) =>
            val destinationMap =
              map
                .get(destinationIdentifier)
                .fold( Map( postDefinition.postId -> postDefinition ) )( dm => dm + (postDefinition.postId -> postDefinition) )
            map + ( destinationIdentifier -> destinationMap )
        case None =>
          println("Attempt to update post definition yielded none, as if the post updated is not known to the server.")
          scala.Predef.identity
  val sideEffectPostUpdate = (mbPostDefinition : Option[PostDefinition]) => currentPostIdentifierManager.set( mbPostDefinition.map( pd => PostIdentifier(destinationIdentifier,pd.postId) ) )
  updateVarFromApiResult[Option[PostDefinition],Map[DestinationIdentifier,Map[Int,PostDefinition]]]( request, backend, destinationsToKnownPostsVar, updater, sideEffectPostUpdate )

def hardUpdatePostDefinitionUpdate(
  protopostLocation : Uri,
  destinationIdentifier : DestinationIdentifier,
  postDefinitionUpdate : PostDefinitionUpdate,
  backend : WebSocketBackend[scala.concurrent.Future],
  destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
  sideEffectPostUpdate : PostDefinition => Unit
)(using ec : ExecutionContext) =
  val request =
    basicRequest
      .post( protopostLocation.addPath("protopost", "update-post") )
      .body( asJson(postDefinitionUpdate) )
      .response( asJson[PostDefinition] )
  val updater : PostDefinition => Map[DestinationIdentifier,Map[Int,PostDefinition]] => Map[DestinationIdentifier,Map[Int,PostDefinition]] = postDefinition =>
    ( map : Map[DestinationIdentifier,Map[Int,PostDefinition]] ) =>
      val destinationMap =
        map
          .get(destinationIdentifier)
          .fold( Map( postDefinition.postId -> postDefinition ) )( dm => dm + (postDefinition.postId -> postDefinition) )
      map + ( destinationIdentifier -> destinationMap )
  updateVarFromApiResult[PostDefinition,Map[DestinationIdentifier,Map[Int,PostDefinition]]]( request, backend, destinationsToKnownPostsVar, updater, sideEffectPostUpdate )

def hardUpdateDestinationsToKnownPosts(
  protopostLocation : Uri,
  destinationIdentifier : DestinationIdentifier,
  backend : WebSocketBackend[scala.concurrent.Future],
  destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]]
)(using ec : ExecutionContext) =
  given Ordering[PostDefinition] = ReverseChronologicalPostDefinitions
  val request =
    basicRequest
      .post( protopostLocation.addPath("protopost", "destination-posts") )
      .body( asJson(destinationIdentifier) )
      .response( asJson[Set[PostDefinition]] )
  val updater : Set[PostDefinition] => Map[DestinationIdentifier,Map[Int,PostDefinition]] => Map[DestinationIdentifier,Map[Int,PostDefinition]] =
    set =>
      val newMap = set.map( pd => (pd.postId, pd) ).toMap
      ( curVal : Map[DestinationIdentifier,Map[Int,PostDefinition]] ) => curVal + Tuple2( destinationIdentifier, newMap )
  updateVarFromApiResult[Set[PostDefinition],Map[DestinationIdentifier,Map[Int,PostDefinition]]]( request, backend, destinationsToKnownPostsVar, updater )

def hardUpdateLoginStatus(
      protopostLocation : Uri,
      backend : WebSocketBackend[scala.concurrent.Future],
      loginStatusVar : Var[Option[(LoginStatus,Long,Boolean)]]
)(using ec : ExecutionContext) : Unit =
  //println("hardUpdateLoginStatus()");
  val request = basicRequest
    .get(protopostLocation.addPath("protopost", "login-status")) // uri"http://localhost:8025/protopost/login-status"
    .response(asJson[LoginStatus])
  val future = request.send(backend).map( _.body ).map( rawBodyToLoginStatusOrThrow )

  future.onComplete:
    case Success(ls) =>
      println("hardUpdateLoginStatus -- success!")
      loginStatusVar.set(Some((ls,epochSecondsNow(),false)))
    case Failure(t) =>
      println("hardUpdateLoginStatus -- failure!")
      t.printStackTrace
      loginStatusVar.set(None)

def decodeOrThrow[T]( rawBody : Either[ResponseException[String], T] ) : T =
  rawBody match
    case Left( oops ) => throw oops //new Exception( oops.toString() )
    case Right( theThing ) => theThing

private val DefaultErrorHandler : Throwable => Unit =
  t =>
    def alert() : Unit = org.scalajs.dom.window.alert( t.toString() )
    println(s"In DefaultErrorHandler, with Throwable ${t}")
    t match
      case re : sttp.client4.ResponseException[?] =>
        dom.console.error( s"ResponseMetadata: ${re.response.toString}" )
        println("ResponseException:")
        println( re.toString() )
      case _ => /* ignore */
    t.printStackTrace()
    t match
      case usce : sttp.client4.ResponseException.UnexpectedStatusCode[?] =>
        if usce.response.code.code / 100 != 4 then alert()
      case _ =>
        alert()

def updateVarFromApiResult[T : JsonValueCodec,U](
      request : Request[Either[ResponseException[String], T]],
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[U],
      updater : T => U => U,
      sideEffectPostUpdate : T => Unit = (_ : T) => (),
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  try
    val future = request.send(backend).map( _.body ).map( decodeOrThrow )

    future.onComplete:
      case Success(result) =>
        //println( s"updateVarFromApiResult - Setting result: ${result}" )
        laminarVar.update(updater(result))
        sideEffectPostUpdate(result)
      case Failure(t) =>
        //println( s"updateVarFromApiResult - Handling error: ${t}" )
        errorHandler(t)
  catch
    case NonFatal(t) => errorHandler(t)


def setVarFromTransformedApiResult[T : JsonValueCodec,U](
      request : Request[Either[ResponseException[String], T]],
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[U],
      transformation : T => U,
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  try
    val future = request.send(backend).map( _.body ).map( decodeOrThrow )

    future.onComplete:
      case Success(result) =>
        //println( s"Setting result: ${result}" )
        laminarVar.set(transformation(result))
      case Failure(t) => errorHandler(t)
  catch
    case NonFatal(t) => errorHandler(t)

def setVarFromApiResult[T : JsonValueCodec](
      request : Request[Either[ResponseException[String], T]],
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[T],
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  setVarFromTransformedApiResult[T,T]( request, backend, laminarVar, scala.Predef.identity, errorHandler )

def setVarFromTransformedApiGetResult[T : JsonValueCodec,U](
      endpointUri : Uri, // will be a get request!
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[U],
      transformation : T => U,
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  val request = basicRequest
    .get(endpointUri)
    .response(asJson[T])
  setVarFromTransformedApiResult( request, backend, laminarVar, transformation, errorHandler )

/*
def setVarFromTransformedApiGetResult[T : JsonValueCodec,U](
      endpointUri : Uri,
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[U],
      transformation : T => U,
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  try
    val request = basicRequest
      .get(endpointUri)
      .response(asJson[T])
    val future = request.send(backend).map( _.body ).map( decodeOrThrow )

    future.onComplete:
      case Success(result) => laminarVar.set(transformation(result))
      case Failure(t) => errorHandler(t)
  catch
    case NonFatal(t) => errorHandler(t)
*/

def setVarFromApiGetResult[T : JsonValueCodec](
      endpointUri : Uri,
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[T],
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  val request = basicRequest
    .get(endpointUri)
    .response(asJson[T])
  setVarFromTransformedApiResult[T,T]( request, backend, laminarVar, transformation = identity, errorHandler )

def setOptionalVarFromApiGetResult[T : JsonValueCodec](
      endpointUri : Uri,
      backend : WebSocketBackend[scala.concurrent.Future],
      laminarVar : Var[Option[T]],
      errorHandler : Throwable => Unit = DefaultErrorHandler
)( using ec : ExecutionContext ) : Unit =
  try
    val request = basicRequest
      .get(endpointUri)
      .response(asJson[T])
    val future = request.send(backend).map( _.body ).map( decodeOrThrow )

    future.onComplete:
      case Success(result) => laminarVar.set(Some(result))
      case Failure(t) =>
        errorHandler(t)
        laminarVar.set(None)
  catch
    case NonFatal(t) =>
      errorHandler(t)
      laminarVar.set(None)

