package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*

import protopost.api.{LoginStatus, given}

import scala.util.{Success,Failure}
//import scala.concurrent.duration.Duration

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
import scala.scalajs.js.timers.*

import scala.annotation.tailrec

// This starting-point laminar application is modified from
// https://github.com/raquo/laminar-full-stack-demo/blob/master/client/src/main/scala/com/raquo/app/basic/HelloWorldView.scala
object Client {

  enum LoginLevel( val colorClass : String ):
    case high extends LoginLevel("logged-in-high")
    case low  extends LoginLevel("logged-in-low")
    case none extends LoginLevel("logged-in-none")

  val loginLevelVar : Var[Option[LoginLevel]] = Var( None )  

  val backend = FetchBackend()
  var loginStatusHandle : Option[SetIntervalHandle] = None

  def maintainLoginStatus() : Unit =
    if loginStatusHandle == None then
      val handle =
        setInterval(60000):
          val request = basicRequest
            .get(uri"http://localhost:8025/protopost/login-status")
            .response(asJson[LoginStatus])
          val future =
            request.send(backend).map( _.body ).map: rawResponse =>
              rawResponse match
                case Left( oops ) => throw oops //new Exception( oops.toString() )
                case Right( loginStatus ) =>
                  println( loginStatus.toString )
                  if loginStatus.highSecurityExpires > 0 then
                    LoginLevel.high
                  else if loginStatus.lowSecurityExpires > 0 then
                    LoginLevel.low
                  else
                    LoginLevel.none

          //val loginStatusStream = EventStream.fromFuture( future )
          future.onComplete:
            case Success(ll) => loginLevelVar.set(Some(ll))
            case Failure(t) =>
              t.printStackTrace
              loginLevelVar.set(None)
      loginStatusHandle = Some( handle )

  def retireLoginStatus() : Unit =
    if loginStatusHandle != null && loginStatusHandle != None then
      clearInterval( loginStatusHandle.get )
      loginStatusHandle = None


  @main
  def main() : Unit =
    lazy val container = dom.document.getElementById("root")
    render(container, top())
    //println("Hello.")

  def top(): HtmlElement = {

    div(
      renderExample(),
      onMountCallback { _ => maintainLoginStatus() },
      onUnmountCallback { _ => retireLoginStatus() }
    )
  }

  def renderExample(): HtmlElement = {

    val nameVar = Var(initial = "world")
    div(
      label("Your name: "),
      input(
        placeholder := "Enter your name here",
        onInput.mapToValue --> nameVar
      ),
      p(
        "Hello, ",
        text <-- nameVar.signal.map(_.toUpperCase)
      ),
      cls <-- loginLevelVar.signal.map( _.fold("logged-in-unknown")( _.colorClass ) )
    )
  }
}
