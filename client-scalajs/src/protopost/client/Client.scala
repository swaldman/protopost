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

    // if the query string includes resetLocalStorage=true, reset the local storage and redirect
    val queryString = dom.window.location.search
    val params = new dom.URLSearchParams(queryString)
    if params.has("resetLocalStorage") then
      val rls = Option(params.get("resetLocalStorage")).map( java.lang.Boolean.parseBoolean )
      if rls == Some(true) then
        LocalStorageItem.resetAll()
        // Remove resetLocalStorage param and redirect to prevent accidental re-clearing on reload
        params.delete("resetLocalStorage")
        val newSearch = params.toString()
        val newUrl = dom.window.location.pathname + (if newSearch.isEmpty then "" else "?" + newSearch)
        dom.window.location.href = newUrl
        return

    lazy val container = dom.document.getElementById("root")
    render( container, TopPanel.create( uri"${Globals.protopostLocation}" ) )

