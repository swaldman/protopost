package protopost.client.util

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import protopost.api.{DestinationIdentifier,PostDefinition,PostDefinitionCreate,PostDefinitionUpdate,given}
import scala.collection.immutable
import protopost.api.PostIdentifier

def epochSecondsNow() : Long = System.currentTimeMillis()/1000

object laminar:
  import org.scalajs.dom
  import com.raquo.laminar.api.L.{*, given}

  val documentEscapeEvents = documentEvents( _.onKeyDown.filter( ke => ke.keyCode == 27 || ke.key == "Escape" || ke.key == "Esc") )
  //val onEscapePress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] = onKeyPress.filter(_.keyCode == dom.KeyCode.Escape) // doesn't reliably work!

// straight from laminar docs
  // https://laminar.dev/documentation
  val onEnterPress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
  

  def blackHr() : HtmlElement =
    hr(
      borderStyle.solid,
      borderColor.black,
      borderWidth.px(1)
    )


object sttp:
  import _root_.sttp.client4.*
  import _root_.sttp.model.*
  import _root_.sttp.client4.fetch.*
  import _root_.sttp.client4.jsoniter.*
  import protopost.api.{LoginStatus,given}
  import protopost.client.{LoginLevel,ReverseChronologicalPostDefinitions}
  import scala.util.{Success,Failure}

  /*
  def rawBodyToLoginLevelOrThrow( rawBody : Either[ResponseException[String], LoginStatus] ) : LoginLevel =
    rawBody match
      case Left( oops ) => throw oops //new Exception( oops.toString() )
      case Right( loginStatus ) => LoginLevel.fromLoginStatus( loginStatus )
  */

  def rawBodyToLoginStatusOrThrow( rawBody : Either[ResponseException[String], LoginStatus] ) : LoginStatus =
    rawBody match
      case Left( oops ) => throw oops //new Exception( oops.toString() )
      case Right( loginStatus ) => loginStatus

  def hardUpdateNewPostDefinition(
    protopostLocation : Uri,
    destinationIdentifier : DestinationIdentifier,
    postDefinitionCreate : PostDefinitionCreate,
    backend : WebSocketBackend[scala.concurrent.Future],
    destinationsToKnownPostsVar : com.raquo.laminar.api.L.Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
    currentPostIdentifierVar : com.raquo.laminar.api.L.Var[Option[PostIdentifier]]
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
    val sideEffectPostUpdate = (mbPostDefinition : Option[PostDefinition]) => currentPostIdentifierVar.set( mbPostDefinition.map( pd => PostIdentifier(destinationIdentifier,pd.postId) ) )
    updateVarFromApiResult[Option[PostDefinition],Map[DestinationIdentifier,Map[Int,PostDefinition]]]( request, backend, destinationsToKnownPostsVar, updater, sideEffectPostUpdate )

  def hardUpdatePostDefinitionUpdate(
    protopostLocation : Uri,
    destinationIdentifier : DestinationIdentifier,
    postDefinitionUpdate : PostDefinitionUpdate,
    backend : WebSocketBackend[scala.concurrent.Future],
    destinationsToKnownPostsVar : com.raquo.laminar.api.L.Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
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
    destinationsToKnownPostsVar : com.raquo.laminar.api.L.Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]]
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
        loginStatusVar : com.raquo.laminar.api.L.Var[Option[(LoginStatus,Long)]]
  )(using ec : ExecutionContext) : Unit =
    //println("hardUpdateLoginStatus()");
    val request = basicRequest
      .get(protopostLocation.addPath("protopost", "login-status")) // uri"http://localhost:8025/protopost/login-status"
      .response(asJson[LoginStatus])
    val future = request.send(backend).map( _.body ).map( rawBodyToLoginStatusOrThrow )

    future.onComplete:
      case Success(ls) =>
        println("hardUpdateLoginStatus -- success!")
        loginStatusVar.set(Some((ls,epochSecondsNow())))
      case Failure(t) =>
        println("hardUpdateLoginStatus -- failure!")
        t.printStackTrace
        loginStatusVar.set(None)

  def decodeOrThrow[T]( rawBody : Either[ResponseException[String], T] ) : T =
    rawBody match
      case Left( oops ) => throw oops //new Exception( oops.toString() )
      case Right( theThing ) => theThing

  private val DefaultErrorHandler : Throwable => Unit =
    t => t.printStackTrace()
    org.scalajs.dom.window.alert( t.toString() )

  def updateVarFromApiResult[T : JsonValueCodec,U](
        request : Request[Either[ResponseException[String], T]],
        backend : WebSocketBackend[scala.concurrent.Future],
        laminarVar : com.raquo.laminar.api.L.Var[U],
        updater : T => U => U,
        sideEffectPostUpdate : T => Unit = (_ : T) => (),
        errorHandler : Throwable => Unit = DefaultErrorHandler
  )( using ec : ExecutionContext ) : Unit =
    try
      val future = request.send(backend).map( _.body ).map( decodeOrThrow )

      future.onComplete:
        case Success(result) =>
          //println( s"Setting result: ${result}" )
          laminarVar.update(updater(result))
          sideEffectPostUpdate(result)
        case Failure(t) => errorHandler(t)
    catch
      case NonFatal(t) => errorHandler(t)


  def setVarFromTransformedApiResult[T : JsonValueCodec,U](
        request : Request[Either[ResponseException[String], T]],
        backend : WebSocketBackend[scala.concurrent.Future],
        laminarVar : com.raquo.laminar.api.L.Var[U],
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
        laminarVar : com.raquo.laminar.api.L.Var[T],
        errorHandler : Throwable => Unit = DefaultErrorHandler
  )( using ec : ExecutionContext ) : Unit =
    setVarFromTransformedApiResult[T,T]( request, backend, laminarVar, scala.Predef.identity, errorHandler )

  def setVarFromTransformedApiGetResult[T : JsonValueCodec,U](
        endpointUri : Uri, // will be a get request!
        backend : WebSocketBackend[scala.concurrent.Future],
        laminarVar : com.raquo.laminar.api.L.Var[U],
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
        laminarVar : com.raquo.laminar.api.L.Var[U],
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
        laminarVar : com.raquo.laminar.api.L.Var[T],
        errorHandler : Throwable => Unit = DefaultErrorHandler
  )( using ec : ExecutionContext ) : Unit =
    val request = basicRequest
      .get(endpointUri)
      .response(asJson[T])
    setVarFromTransformedApiResult[T,T]( request, backend, laminarVar, transformation = identity, errorHandler )

  def setOptionalVarFromApiGetResult[T : JsonValueCodec](
        endpointUri : Uri,
        backend : WebSocketBackend[scala.concurrent.Future],
        laminarVar : com.raquo.laminar.api.L.Var[Option[T]],
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
