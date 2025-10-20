package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object ClickLink:
  val baseModifiers = Seq(
      cls("click-link"),
      cursor.default,
  )

  def create( linkText : String ) : HtmlElement =
    a(
      baseModifiers,
      linkText
    )

  def create( linkTextSignal : Signal[String] ) : HtmlElement =
    a(
      baseModifiers,
      text <-- linkTextSignal
    )


