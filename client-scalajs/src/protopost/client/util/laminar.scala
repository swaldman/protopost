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

