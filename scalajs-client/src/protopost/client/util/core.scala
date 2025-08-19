package protopost.client.util

import scala.concurrent.ExecutionContext




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

  def rawBodyToLoginLevelOrThrow( rawBody : Either[ResponseException[String], LoginStatus] ) : LoginLevel =
    rawBody match
      case Left( oops ) => throw oops //new Exception( oops.toString() )
      case Right( loginStatus ) =>
        //println( loginStatus.toString )
        if loginStatus.highSecuritySecondsRemaining > 0 then
          LoginLevel.high
        else if loginStatus.lowSecuritySecondsRemaining > 0 then
          LoginLevel.low
        else
          LoginLevel.none

  def updateLoginStatus(protopostLocation : Uri, backend : WebSocketBackend[scala.concurrent.Future], loginLevelVar : com.raquo.laminar.api.L.Var[Option[LoginLevel]])(using ec : ExecutionContext) : Unit =
    val request = basicRequest
      .get(protopostLocation.addPath("protopost", "login-status")) // uri"http://localhost:8025/protopost/login-status"
      .response(asJson[LoginStatus])
    val future = request.send(backend).map( _.body ).map( rawBodyToLoginLevelOrThrow )

    future.onComplete:
      case Success(ll) => loginLevelVar.set(Some(ll))
      case Failure(t) =>
        t.printStackTrace
        loginLevelVar.set(None)
