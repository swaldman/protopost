package protopost.client.util

import scala.concurrent.ExecutionContext

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

  def hardUpdateLoginStatus(protopostLocation : Uri, backend : WebSocketBackend[scala.concurrent.Future], loginStatusVar : com.raquo.laminar.api.L.Var[Option[(LoginStatus,Long)]])(using ec : ExecutionContext) : Unit =
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
