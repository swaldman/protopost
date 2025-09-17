package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object TinyLink:
  def create( linkText : String ) : HtmlElement =
    a(
      cls("tiny-link"),
      fontSize.pt(9),
      cursor.default,
      linkText
    )


