package protopost.client

import protopost.api.{PosterNoAuth, given}
import sttp.model.Uri

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object ProfilePanel:
  def create(protopostLocation : Uri, loginLevelSignal : Signal[Option[LoginLevel]]) : HtmlElement =
    val loginEvents = loginLevelSignal.changes.filter( _.fold(false)(level => level != LoginLevel.none) ).map( _.get )
    val posterNoAuthVar : Var[Option[PosterNoAuth]] = Var(None)

    val loginObserver = Observer[LoginLevel]: _ =>
      util.sttp.setOptionalVarFromApiResult[PosterNoAuth]( protopostLocation.addPath("protopost", "poster-info"), Client.backend, posterNoAuthVar )

    div(
      backgroundColor("#ccccff"),
      width("100%"),
      height("100%"),
      div(
        idAttr := "profile-name-pane",
        fontSize("18pt"),
        fontWeight("bold"),
        text <-- posterNoAuthVar.signal.map: mbPna =>
          mbPna match
            case Some( pna ) => s"Hello, ${pna.fullName}!"
            case None => ""
      ),
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        loginEvents.addObserver(loginObserver)
      }
    )
