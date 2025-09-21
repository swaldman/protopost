package protopost.client

import protopost.common.api.{DestinationIdentifier,PosterNoAuth,PostDefinition,PostDefinitionUpdate,PostIdentifier,given}

import util.laminar.{documentEscapeEvents,onEnterPress}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.common.api.{PostDefinitionCreate,UpdateValue}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object CurrentPostCard:
  private val NoPostChosenLabel = "No post chosen"
  def create(
    protopostLocation : Uri,
    backend : WebSocketBackend[scala.concurrent.Future],
    destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
    currentPostIdentifierLsi : LocalStorageItem[Option[PostIdentifier]],
    currentPostDefinitionSignal : Signal[Option[PostDefinition]],
    currentPostLocalContentTypeLsi : LocalStorageItem[String],
    currentPostLocalTextLsi : LocalStorageItem[String],
    posterNoAuthSignal : Signal[Option[PosterNoAuth]]
  ) : HtmlElement =
    val currentPostIdentifierSignal = currentPostIdentifierLsi.signal
    val userIsOwnerSignal : Signal[Boolean] = Signal.combine(posterNoAuthSignal,currentPostDefinitionSignal).map: (mbPna, mbCpd) =>
      val mbOut =
        for
          pna <- mbPna
          cpd <- mbCpd
        yield
          pna.id == cpd.owner.id
      //println(s"(mbPna, mbCpd, mbOut) ($mbPna, $mbCpd, $mbOut)")
      mbOut.getOrElse(false)

    val titleDirtyVar = Var(false)
    val titleBackgroundColorSignal = titleDirtyVar.signal.map( if _ then "yellow" else "white" )

    val titleChangeEventBus = new EventBus[HtmlElement]
    val titleChangeEventStreamWithCurrentPost = titleChangeEventBus.events.withCurrentValueOf(currentPostDefinitionSignal)
    val titleChangeObserver = Observer[(HtmlElement, Option[PostDefinition])]: ( node, mbPd ) =>
      //println(s"( node, mbPd ): ( ${node}, $mbPd )   ${java.time.Instant.now()}")
      mbPd match
        case Some(pd) =>
          val newTextOpt =
             val tmp = node.ref.textContent.trim
             if tmp.length == 0 then None else Some(tmp)
          val currentOpt = pd.title
          if newTextOpt != currentOpt then
            val pdu = PostDefinitionUpdate( pd.postId, title = newTextOpt.fold( UpdateValue.`set-to-none` )( UpdateValue.`update`.apply ) )
            titleDirtyVar.set(true)
            util.sttp.hardUpdatePostDefinitionUpdate(protopostLocation,pd.destination.destinationIdentifier, pdu, backend, destinationsToKnownPostsVar, _ => titleDirtyVar.set(false))
        case None =>
          println("No post definition to update title of. Try to prevent any capacity to edit title.")
      node.ref.blur()

    val postIdentifierObserver = Observer[(Option[PostIdentifier],Map[DestinationIdentifier,Map[Int,PostDefinition]])]: (mbpi, map) =>
      mbpi.foreach: pi =>
        val di = pi.destinationIdentifier
        val pd =
          for
            dm <- map.get(di)
            pd <- dm.get(pi.postId)
          yield pd
        if pd.isEmpty then  
          util.sttp.hardUpdateDestinationsToKnownPosts( protopostLocation, pi.destinationIdentifier, backend, destinationsToKnownPostsVar )

    val composePane = ComposerPane.create( currentPostLocalContentTypeLsi, currentPostLocalTextLsi )

    div(
      idAttr := "current-post-card",
      paddingLeft.rem(Client.CardPaddingLeftRightRem),
      paddingRight.rem(Client.CardPaddingLeftRightRem),
      height.percent(100),
      div(
        idAttr := "current-post-card-no-post",
        display <-- currentPostDefinitionSignal.map( opt => if opt.isEmpty then "block" else "none" ),
        NoPostChosenLabel
      ),
      div(
        idAttr := "current-post-card-with-post",
        height.percent(100),
        display <-- currentPostDefinitionSignal.map( opt => if opt.isEmpty then "none" else "block" ),
        div(
          idAttr := "current-post-title",
          span(
            fontSize.pt(Client.CardTitleFontSizePt),
            fontWeight.bold,
            contentEditable <-- userIsOwnerSignal,
            backgroundColor <-- titleBackgroundColorSignal,
            text <-- currentPostDefinitionSignal.map( _.map( _.title.getOrElse(Client.UntitledPostLabel) ).getOrElse("") ),
            inContext { thisNode =>
              //onEnterPress.preventDefault.mapTo(thisNode) --> titleChangeEventBus
              onEnterPress.preventDefault --> { whatever =>
                thisNode.ref.blur()
              }
            },
            inContext { thisNode =>
              onBlur.mapTo(thisNode) --> titleChangeEventBus
            },
            inContext { thisNode =>
              documentEscapeEvents.compose( _.withCurrentValueOf(currentPostDefinitionSignal) ) --> { (_,mbCpd) =>
                if thisNode.ref == dom.document.activeElement then
                  thisNode.ref.textContent = mbCpd.fold( NoPostChosenLabel )( cpd => cpd.title.getOrElse(Client.UntitledPostLabel) )
                  thisNode.ref.blur()
              }
            }
          ),
          " ",
          span(
            color.gray,
            fontStyle.italic,
            display <-- userIsOwnerSignal.map( if _ then "inline" else "none" ),
            "\u2190 edit the title here"
          )
        ),
        div(
          idAttr := "current-post-compose",
          height.percent(100),
          composePane
        )
      ),
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        given Owner = mountContext.owner
        titleChangeEventStreamWithCurrentPost.addObserver( titleChangeObserver )
        currentPostIdentifierSignal.withCurrentValueOf(destinationsToKnownPostsVar).addObserver( postIdentifierObserver )
      },
    )

