package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object TableOfContentsPanel:
  def create() : HtmlElement =
    div(
      idAttr("toc"),
    )

