package protopost.client.util.laminar

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

val documentEscapeEvents = documentEvents( _.onKeyDown.filter( ke => ke.keyCode == 27 || ke.key == "Escape" || ke.key == "Esc") )
//val onEscapePress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] = onKeyPress.filter(_.keyCode == dom.KeyCode.Escape) // doesn't reliably work!

// straight from laminar docs
// https://laminar.dev/documentation
val onEnterPress: EventProcessor[dom.KeyboardEvent, dom.KeyboardEvent] = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)

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


