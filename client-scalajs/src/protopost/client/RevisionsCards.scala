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


object RevisionsCards:
  enum Card:
    case noRevisionHistoryLoaded, revisionHistoryList, revisionPreview

  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

    val selectedRevisionVar : Var[Option[RevisionTimestamp]] = Var(None)

    val previewRevisionVar : Var[Option[RetrievedPostRevision]] = Var(None)

    val viewSourceVar : Var[Boolean] = Var(false)
    val viewSourceSignal = viewSourceVar.signal

    val loadPreviewRevisionObserver = Observer[(RevisionTimestamp,PostDefinition)]: (rt,pd) =>
      util.request.loadPostRevision(
        protopostLocation,
        pd.postId,
        rt,
        backend,
        previewRevisionVar
      )

    val innerHtmlRevisionSignal : Signal[Option[String]] =
      previewRevisionVar.signal.map: mbrpr =>
        mbrpr.map: rpr =>
          rpr.contentType match
            case "text/plain" =>
              div(
                whiteSpace.pre,
                fontFamily("monospace"),
                rpr.body
              ).ref.outerHTML
            case "text/html" =>
              safeHtmlFromUserHtml( rpr.body )
            case "text/markdown" => 
              safeHtmlFromMarkdown( rpr.body )
            case other =>
              s"""<b style="color: red">Unexpected revision content-type: $other</b>"""

    val cardSignal =
      Signal.combine(currentPostAllRevisionsSignal,selectedRevisionVar).map: tup =>
        tup match
          case ( Some(_), Some(_) ) =>
            Card.revisionPreview
          case ( Some(_), None )    =>
            Card.revisionHistoryList
          case ( None, Some(_) )   =>
            println( "RevisionsCard: Inconsistent: No RevisionHistory, but selected revision" )
            Card.noRevisionHistoryLoaded
          case ( None, None )   =>  
            Card.noRevisionHistoryLoaded

    val currentPostAllRevisionsLinks =
      val direct = currentPostAllRevisionsSignal.map( _.fold(Seq.empty[RevisionTimestamp])(_.revisionTimestampReverseChronological) )
      def makeRevisionLink( rt : RevisionTimestamp, initial : RevisionTimestamp, signal : Signal[RevisionTimestamp] ) =
        // this is unusual, we can just ignore the update signal, because the revision timestamp for an id (revision timestamp) is
        // unique and invariant
        div(
          TinyLink.create( Client.RevisionTimestampFormatter.format(rt.asInstant) ).amend(
            onClick --> { _ => selectedRevisionVar.set(Some(rt)) }
          )
        )
      direct.split( identity )( makeRevisionLink ) // we'll only construct anew if we don't already have one for revision ID

    /*
      // naive, without the caching/retention provided by split...
      // see https://laminar.dev/documentation#lists-of-children
      direct.map: srt =>
        srt.map : rt =>
          div(
            TinyLink.create( RevisionTimestampFormatter.format(rt.asInstant) ).amend(
              onClick --> { _ => selectedRevisionVar.set(Some(rt)) }
            )
          )
     */

    val noRevisionHistoryLoadedCard =
      div(
        display <-- cardSignal.map( card => if card == Card.noRevisionHistoryLoaded then "block" else "none" ),
        em(
          "No revision history loaded."
        )
      )

    val revisionHistoryListCard =
      div(
          display <-- cardSignal.map( card => if card == Card.revisionHistoryList then "block" else "none" ),
          div(
            PublishDetailsPaneLabelCommonModifiers,
            "revisions:"
          ),
          div(
            sectionBorderPaddingMargin,
            idAttr := "revisons-cards-timestamp-list",
            children <-- currentPostAllRevisionsLinks
          )
      )

    val revisionPreviewCard =
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
        display <-- cardSignal.map( card => if card == Card.revisionPreview then "flex" else "none" ),
        flexDirection.column,
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "revision preview:"
        ),
        div(
          idAttr := "revisons-cards-preview",
          sectionBorderPaddingMargin,
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
                selectedRevisionVar.set(None)
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
              "make current",
              onClick( _.withCurrentValueOf(previewRevisionVar) ) --> { tup =>
                tup match
                case ( _, Some( rpr ) ) =>
                  val pc = PostContent(rpr.contentType, rpr.body)
                  currentPostLocalPostContentLsi.set(pc)
                  selectedRevisionVar.set(None)
                  previewRevisionVar.set(None)
                  localContentDirtyVar.set(true)
                  //composerPaneCurrentTabVar.set(ComposerPane.Tab.edit)
                  resetComposersToEdit()
                  viewSourceVar.set(false)
                case _ => /* ignore */
              }
            ),
          ),
          child <-- viewSourceSignal.map( vs => if vs then viewSourceCard else formattedCard )
        )
      )

    div(
      noRevisionHistoryLoadedCard,
      revisionHistoryListCard,
      revisionPreviewCard,
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        currentPostDefinitionChangeEvents.filter( _.isEmpty).addObserver( Observer[Option[PostDefinition]]( _ => selectedRevisionVar.set(None) ) )
        val loadRevisionEventStream =
          selectedRevisionVar.signal.withCurrentValueOf(currentPostDefinitionSignal).changes
            .map( (mba,mbb) => mba.flatMap(a => mbb.map(b=>(a,b))) )
            .filter( _.nonEmpty )
            .map(_.get)
        loadRevisionEventStream.addObserver( loadPreviewRevisionObserver )
      }
    )
