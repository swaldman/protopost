package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object ClickLink:
  def create( linkText : String ) : HtmlElement =
    a(
      cls("click-link"),
      cursor.default,
      linkText
    )


