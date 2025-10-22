package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import sttp.model.Uri
import protopost.common.api.PostRevisionHistory
import com.raquo.laminar.modifiers.KeySetter
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLElement
import protopost.common.api.{PostDefinition,RevisionTimestamp}
import protopost.common.api.RetrievedPostRevision
import protopost.client.util.safeHtmlFromUserHtml
import protopost.client.util.safeHtmlFromMarkdown
import sttp.client4.WebSocketBackend

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*


// This is very much not DRY, modified from RevisionsCard
// Try to factor out commonalities!
object UnsavedRevisionPreviewCard:

  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

    def copyTextToClipboardAndJump(mbur : Option[UnsavedRevision]) : Unit =
      mbur match
        case Some( ur ) =>
          if dom.window.navigator.clipboard != null then
            dom.window.navigator.clipboard.writeText(ur.newPostRevision.body)
              .toFuture
              .onComplete:
                case scala.util.Success(_) =>
                  currentPostIdentifierManager.set( Some( ur.postIdentifier ) )
                  topPanelLocationLsi.set(TopPanel.Tab.currentPost)
                case scala.util.Failure(e) =>
                  dom.window.alert(s"Failed to copy text: ${e.getMessage}")
          else
            dom.window.alert("Clipboard API not available.")
        case None =>
          dom.window.alert("No UnsavedRevision selected whose text to copy or PostIdentifier to jump to!")

    val nprSignal = selectedUnsavedRevisionSignal.map( _.map( _.newPostRevision ) )

    val viewSourceVar : Var[Boolean] = Var(false)
    val viewSourceSignal = viewSourceVar.signal

    val innerHtmlRevisionSignal : Signal[Option[String]] =
      nprSignal.map: mbnpr =>
        mbnpr.map: npr =>
          npr.contentType match
            case "text/plain" =>
              div(
                whiteSpace.pre,
                fontFamily("monospace"),
                npr.body
              ).ref.outerHTML
            case "text/html" =>
              Globals.html_beautify( safeHtmlFromUserHtml( npr.body ) )
            case "text/markdown" => 
              safeHtmlFromMarkdown( npr.body )
            case other =>
              s"""<b style="color: red">Unexpected revision content-type: $other</b>"""

    val commonModifiers = Seq(
        marginTop.rem(0.5),
        borderTopWidth.px(2),
        borderTopColor.black,
        borderTopStyle.solid,
        paddingTop.rem(0.5),
        overflowX.scroll,
    )
    val formattedCard =
      div(
        commonModifiers,
        fontFamily(serifFontFamilies),
        inContext { thisNode =>
          innerHtmlRevisionSignal --> { (mbHtml) => thisNode.ref.innerHTML = mbHtml.getOrElse("<b>No revision loaded</b>") }
        }
      )
    val viewSourceCard =
      div(
        commonModifiers,
        whiteSpace.pre,
        fontFamily("monospace"),
        text <-- innerHtmlRevisionSignal.map( _.getOrElse("No revision loaded.") )
      )

    div(
      idAttr := "unsaved-revisons-cards-preview",
      display.flex,
      flexDirection.column,
      div(
        //borderStyle.solid,
        //borderWidth.px(2),
        //borderColor.aqua,
        display.flex,
        button(
          cls := "button-utilitarian",
          role("button"),
          "back",
          onClick --> { _ =>
            selectedUnsavedRevisionVar.set(None)
            viewSourceVar.set(false)
          }
        ),
        div(
          flexGrow(1),
          textAlign.center,
          button(
            cls := "button-utilitarian",
            role("button"),
            text <-- viewSourceSignal.map( vs => if vs then "view preview" else "view source" ),
            onClick --> { _ => viewSourceVar.update(!_) }
          ),
        ),
        button(
          cls := "button-utilitarian",
          role("button"),
          "copy and jump",
          onClick( _.withCurrentValueOf(selectedUnsavedRevisionVar) ) --> { tup =>
            copyTextToClipboardAndJump( tup(1) )
            selectedUnsavedRevisionVar.set(None)
          }
        ),
      ),
      child <-- viewSourceSignal.map( vs => if vs then viewSourceCard else formattedCard )
    )
