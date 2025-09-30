package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.util.{Success,Failure}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import protopost.common.api.{EmailPassword,LoginStatus,given}
import protopost.common.{EmailAddress,Password}
import org.scalajs.dom.KeyboardEvent

import protopost.client.util.epochSecondsNow
import protopost.client.util.request.rawBodyToLoginStatusOrThrow
import protopost.client.util.laminar.onEnterPress

object LoginForm:

  object Color:
    val GoodField = "#aaddaa"
    val BadField  = "#ffaaaa"

  case class Prerequisites(loginStatusVar : Var[Option[(LoginStatus, Long)]], loginLevelSignal : Signal[LoginLevel], loginLevelChangeEvents : EventStream[LoginLevel])

  def create(protopostLocation : Uri, backend : WebSocketBackend[scala.concurrent.Future], prerequisites : LoginForm.Prerequisites) : HtmlElement =
    create(protopostLocation, backend, prerequisites.loginStatusVar, prerequisites.loginLevelSignal, prerequisites.loginLevelChangeEvents)

  def create(protopostLocation : Uri, backend : WebSocketBackend[scala.concurrent.Future], loginStatusVar : Var[Option[(LoginStatus, Long)]], loginLevelSignal : Signal[LoginLevel], loginLevelChangeEvents : EventStream[LoginLevel]) : HtmlElement =
    val emailVar = Var[String]("")
    val passwordVar = Var[String]("")
    val emailPasswordSignal = emailVar.signal.combineWith(passwordVar)
    val loginFormMessage = Var[String]("")

    val emailBackgroundStream =
      emailVar.signal.changes /* .debounce(300) */
        .map: s =>
          if s.isEmpty then ""
          else if EmailAddress.isValidEmail(s) then Color.GoodField
          else Color.BadField

    val inputResetStringsStream = loginLevelChangeEvents.map( _ => "" )

    val submitter = Observer[(KeyboardEvent, String, String)]: tuptup =>

      def refreshLoginStatus() : Unit =
        loginStatusVar.set(None)
        protopost.client.util.request.hardUpdateLoginStatus(protopostLocation, backend, loginStatusVar)

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
        require( p.nonEmpty, "No password has been provided." )
        val email = EmailAddress(e)
        val password = Password(p)
        val emailPassword = EmailPassword(email,password)
        val loginEndpoint = protopostLocation.addPath("protopost","login")
        val request =
          basicRequest
            .post(loginEndpoint)
            .body(asJson(emailPassword))
            .response(asJson[LoginStatus])
        val future = request.send(backend)
        future.onComplete:
          case Success(response) =>
            if response.code.code == 401 || response.code.code == 403 then // authentication failure
              loginFormMessage.set( "Authentification failed: " + trimErrorBody( extractErrorBody(response) ) )
              refreshLoginStatus()
            else if response.code.isSuccess then
              val ls = rawBodyToLoginStatusOrThrow(response.body)
              loginStatusVar.set(Some(Tuple2(ls,epochSecondsNow())))
            else
              loginFormMessage.set( "Something went wrong:" + trimErrorBody( extractErrorBody(response) ) )
              refreshLoginStatus()
          case Failure(t) =>
            loginFormMessage.set("Failed to get a response: " + t.toString())
            refreshLoginStatus()
      catch
        case e : Exception =>
          loginFormMessage.set(e.getMessage())

    end submitter

    val disabledSignal : Signal[Boolean] =
      loginLevelSignal.map:
        case LoginLevel.unknown => true
        case LoginLevel.high    => true
        case _ => false

    div(
      // main login pane
      width.percent(100),
      height.percent(100),
      display.flex,
      alignItems.center,
      justifyContent.center,
      flexDirection.column,
      div(
        // username password form
        div(
          // username
          input(
            placeholder("e-mail address"),
            disabled <-- disabledSignal,
            onInput.mapToValue.map( _.trim ) --> emailVar, //e-mail addresses (without display part) contain no whitespaces

            // trying to make the color change on autocomplete, but it looks like Brave's internal stylesheet
            // insists upon its post-autofill color
            //onChange.mapToValue.map( _.trim ) --> emailVar, // can't seem to handle autofill properly
            //onBlur.mapToValue.map( _.trim ) --> emailVar, // can't seem to handle autofill properly

            onInput.mapTo("") --> loginFormMessage,
            backgroundColor <-- emailBackgroundStream,
            value <-- inputResetStringsStream,
            backgroundColor <-- inputResetStringsStream,
            onEnterPress.compose( _.withCurrentValueOf(emailPasswordSignal) ) --> submitter,
            size(32),
          )
        ),
        div(
          // password
          input(
            `type` := "password",
            placeholder("password"),
            onInput.mapToValue --> passwordVar,
            disabled <-- disabledSignal,
            onInput.mapTo("") --> loginFormMessage,
            value <-- inputResetStringsStream,
            onEnterPress.compose( _.withCurrentValueOf(emailPasswordSignal) ) --> submitter,
            size(32),
          )
        ),
      ),
      div(
        // error messages
        marginTop.em(0.5),
        color.red,
        fontSize.pt(8),
        height.em(1.5),
        text <-- loginFormMessage
      ),
    )
