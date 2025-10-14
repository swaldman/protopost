package protopost.client.util.laminar

import scala.scalajs.js
import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

val documentEscapeEvents =
  // autocomplete sometimes causes emission of ill-formed key events
  // at the document level, so we cautiously guard against these
  def guardedCheckKeyEvent( ke : dom.KeyboardEvent ) : Boolean =
    val keDyn = ke.asInstanceOf[js.Dynamic]
    def goodByCode =
      val keyCodeOrUndefined = keDyn.keyCode
      (scala.scalajs.js.typeOf(keyCodeOrUndefined) == "number" && keyCodeOrUndefined.asInstanceOf[Int] == 27)
    def goodByKey =
      val keyOrUndefined = keDyn.key
      if scala.scalajs.js.typeOf(keyOrUndefined) == "string" then
        val keyStr = keyOrUndefined.toString()
        (keyStr == "Escape" || keyStr == "Esc")
      else
        false
    goodByCode || goodByKey
  documentEvents( _.onKeyDown.filter( guardedCheckKeyEvent ) )

//val onEscapePress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] = onKeyPress.filter(_.keyCode == dom.KeyCode.Escape) // doesn't reliably work!

val onEnterPress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] =
  onKeyPress.filter( ke => ke.keyCode == dom.KeyCode.Enter || ke.key == "Enter" )

def blackHr() : HtmlElement =
  hr(
    borderStyle.solid,
    borderColor.black,
    borderWidth.px(1)
  )

def statusCircle() : HtmlElement =
  div(
    backgroundColor.yellow,
    borderRadius.percent(50),
    width.rem(0.75),
    height.rem(0.75),
    borderColor.black,
    borderWidth.px(1),
    borderStyle.solid
  )

def tabsDiv[T](id : String, values : Array[T], currentTabVar : Var[T], currentTabSignal : Signal[T]) =
  val tabs =
    values.map: tab =>
      div(
        cls := "tab-pane",
        flexGrow(1),
        textAlign.center,
        cls <-- currentTabSignal.map( currentTab => if tab == currentTab then "current" else "" ),
        protopost.client.TinyLink.create(tab.toString).amend(
          onClick --> ( click => currentTabVar.set(tab) ),
        )
      )
  div(
    idAttr := id,
    display.flex,
    flexDirection.row,
    alignItems.center,
    justifyContent.center,
    tabs
  )


