package protopost.client

import protopost.api.{PosterNoAuth, given}
import sttp.model.Uri

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object ProfilePanel:
  def create(loginLevelChangeEvents : EventStream[LoginLevel], loginObserver : Observer[LoginLevel], posterNoAuthVar : Var[Option[PosterNoAuth]]) : HtmlElement =
    div(
      idAttr("profile-panel"),
      //backgroundColor("#ccccff"),
      //boxSizing.borderBox,
      width.percent(100),
      height.percent(100),
      paddingLeft.rem(0.5),
      paddingRight.rem(0.5),
      borderWidth.px(3),
      borderColor.black,
      // margin.rem(1.5),
      // display.flex,
      // flexDirection.column,
      div(
        idAttr := "profile-name-pane",
        fontSize.pt(18),
        fontWeight.bold,
        text <-- posterNoAuthVar.signal.map: mbPna =>
          mbPna match
            case Some( pna ) => s"Hello, ${pna.fullName}!"
            case None => ""
      ),
      hr(
        borderStyle.solid,
        borderColor.black,
        borderWidth.px(1)
      ),
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        loginLevelChangeEvents.addObserver(loginObserver)
      }
    )
