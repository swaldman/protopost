package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.model.*

object Client:
  val CardPaddingLeftRightRem = 0.5d
  val CardTitleFontSizePt = 18
  val CardSectionTitleFontSizePt = 14
  val CardBaseTextSizePt = 12

  val UntitledPostLabel = "(untitled post)"

  val DefaultComposer = Composer.`text-and-preview`

  @main
  def main() : Unit =
    lazy val container = dom.document.getElementById("root")
    render( container, TopPanel.create( uri"${Globals.protopostLocation}" ) )

