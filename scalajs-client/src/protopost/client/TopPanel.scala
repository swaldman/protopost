package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.util.{Success,Failure}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
import scala.scalajs.js.timers.*

import protopost.api.{LoginStatus, given}
import protopost.client.util.sttp.rawBodyToLoginLevelOrThrow

object TopPanel:
  def create(protopostLocation : Uri) : HtmlElement =

    val loginLevelVar : Var[Option[LoginLevel]] = Var( None )  
    var loginStatusHandle : Option[SetIntervalHandle] = None

    val loginForm = LoginForm.create(protopostLocation, loginLevelVar)

    def updateLoginStatus() : Unit =
      protopost.client.util.sttp.updateLoginStatus(protopostLocation, Client.backend, loginLevelVar)

    def maintainLoginStatus() : Unit =
      updateLoginStatus()
      if loginStatusHandle == None then
        val handle =
          setInterval(60000):
            updateLoginStatus()
        loginStatusHandle = Some( handle )

    def retireLoginStatus() : Unit =
      if loginStatusHandle != null && loginStatusHandle != None then
        clearInterval( loginStatusHandle.get )
        loginStatusHandle = None

    div(
      onMountCallback { _ => maintainLoginStatus() },
      onUnmountCallback { _ => retireLoginStatus() },
      idAttr("top"),
      cls <-- loginLevelVar.signal.map( _.fold("logged-in-unknown")( _.colorClass ) ),
      loginForm
    )

