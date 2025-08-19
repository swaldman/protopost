package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.util.{Success,Failure}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import protopost.api.{EmailPassword,LoginStatus,given}
import protopost.{EmailAddress,Password}
import org.scalajs.dom.KeyboardEvent

import protopost.client.util.sttp.rawBodyToLoginLevelOrThrow
import protopost.client.util.laminar.onEnterPress

object LoginForm:
  def create(protopostLocation : Uri, loginLevelVar : Var[Option[LoginLevel]]) : HtmlElement =
    val emailVar = Var[String]("")
    val passwordVar = Var[String]("")
    val emailPasswordSignal = emailVar.signal.combineWith(passwordVar)
    val loginFormMessage = Var[String]("")

    val emailBackgroundStream =
      emailVar.signal.changes /* .debounce(300) */
        .map: s =>
          if s.isEmpty then ""
          else if EmailAddress.isValidEmail(s) then "good-field"
          else "bad-field"

    val submitter = Observer[(KeyboardEvent, String, String)]: tuptup =>

      def refreshLoginStatus() : Unit =
        loginLevelVar.set(None)
        protopost.client.util.sttp.updateLoginStatus(protopostLocation, Client.backend, loginLevelVar)
        
      def extractErrorBody[T]( response : Response[Either[ResponseException[String],T]] ) : String =
        response.body match
          case Left( re : ResponseException[String] ) =>
            re match
              case usc : ResponseException.UnexpectedStatusCode[String] => usc.body
              case de : ResponseException.DeserializationException => de.body
          case Right( bodyObj ) =>
            s"Unexpected apparent success? ${bodyObj}"

      def trimErrorBody( eb : String ) : String =
        val first = eb.trim.linesIterator.next()
        first.stripSuffix("at").trim

      try
        val ( _, e, p) = tuptup
        val email = EmailAddress(e)
        val password = Password(p)
        val emailPassword = EmailPassword(email,password)
        val loginEndpoint = protopostLocation.addPath("protopost","login")
        val request =
          basicRequest
            .post(loginEndpoint)
            .body(asJson(emailPassword))
            .response(asJson[LoginStatus])
        val future = request.send(Client.backend)
        future.onComplete:
          case Success(response) =>
            if response.code.code == 401 || response.code.code == 403 then // authentication failure
              loginFormMessage.set( "Authentification failed: " + trimErrorBody( extractErrorBody(response) ) )
              refreshLoginStatus()
            else if response.code.isSuccess then
              val ll = rawBodyToLoginLevelOrThrow(response.body)
              loginLevelVar.set(Some(ll))
            else
              loginFormMessage.set( "Something went wrong:" + trimErrorBody( extractErrorBody(response) ) )
              refreshLoginStatus()
          case Failure(t) =>
            loginFormMessage.set("Failed to get a response: " + t.toString())
            refreshLoginStatus()
      catch
        case e : Exception =>
          loginFormMessage.set(e.toString())

    end submitter

    val disabledSignal : Signal[Boolean] =
      loginLevelVar.signal.map:
        case None => true
        case Some( LoginLevel.high ) => true
        case _ => false

    div(
      cls("login-form"),
      div(
        cls("login-form-inputs"),
        div(
          input(
            placeholder("e-mail address"),
            disabled <-- disabledSignal,
            onInput.mapToValue --> emailVar,
            onInput.mapTo("") --> loginFormMessage,
            cls <-- emailBackgroundStream,
            onEnterPress.compose( _.withCurrentValueOf(emailPasswordSignal) ) --> submitter,
            size(32),
          )
        ),
        div(
          input(
            `type` := "password",
            placeholder("password"),
            onInput.mapToValue --> passwordVar,
            disabled <-- disabledSignal,
            onInput.mapTo("") --> loginFormMessage,
            onEnterPress.compose( _.withCurrentValueOf(emailPasswordSignal) ) --> submitter,
            size(32),
          )
        ),
      ),
      div(
        cls := "login-form-message",
        text <-- loginFormMessage
      )
    )
