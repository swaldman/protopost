package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.model.*

object Client:
  final val TinyLinkFontSize = 9 //9pt

  @main
  def main() : Unit =
    lazy val container = dom.document.getElementById("root")
    render( container, TopPanel.create( uri"${Globals.protopostLocation}" ) )

