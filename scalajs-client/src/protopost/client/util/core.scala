package protopost.client.util

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

def epochSecondsNow() : Long = System.currentTimeMillis()/1000

object laminar:
  import org.scalajs.dom
  import com.raquo.laminar.api.L.{*, given}

  // straight from laminar docs
  // https://laminar.dev/documentation
  val onEnterPress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)

object sttp:
  import _root_.sttp.client4.*
  import _root_.sttp.model.*
  import _root_.sttp.client4.fetch.*
  import _root_.sttp.client4.jsoniter.*
  import protopost.api.{LoginStatus,given}
  import protopost.client.LoginLevel
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
      case Success(ls) => loginStatusVar.set(Some((ls,epochSecondsNow())))
      case Failure(t) =>
        t.printStackTrace
        loginStatusVar.set(None)

  def decodeOrThrow[T]( rawBody : Either[ResponseException[String], T] ) : T =
    rawBody match
      case Left( oops ) => throw oops //new Exception( oops.toString() )
      case Right( theThing ) => theThing

  private val DefaultErrorHandler : Throwable => Unit = t => t.printStackTrace()    

  def setVarFromApiResult[T : JsonValueCodec](
        endpointUri : Uri,
        backend : WebSocketBackend[scala.concurrent.Future],
        laminarVar : com.raquo.laminar.api.L.Var[T],
        errorHandler : Throwable => Unit = DefaultErrorHandler
  )( using ec : ExecutionContext ) : Unit =
    try
      val request = basicRequest
        .get(endpointUri)
        .response(asJson[T])
      val future = request.send(backend).map( _.body ).map( decodeOrThrow )

      future.onComplete:
        case Success(result) => laminarVar.set(result)
        case Failure(t) => errorHandler(t)
    catch
      case NonFatal(t) => errorHandler(t)

  def setOptionalVarFromApiResult[T : JsonValueCodec](
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
