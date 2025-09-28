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
    currentPostLocalPostContentLsi : LocalStorageItem[PostContent],
    localContentDirtyVar : Var[Boolean],
    posterNoAuthSignal : Signal[Option[PosterNoAuth]],
    manualSaveWriteBus : WriteBus[Unit]
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
            util.request.hardUpdatePostDefinitionUpdate(protopostLocation,pd.destination.destinationIdentifier, pdu, backend, destinationsToKnownPostsVar, _ => titleDirtyVar.set(false))
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
          util.request.hardUpdateDestinationsToKnownPosts( protopostLocation, pi.destinationIdentifier, backend, destinationsToKnownPostsVar )

    val composePane = ComposerPane.create( currentPostLocalPostContentLsi, localContentDirtyVar, manualSaveWriteBus )

    div(
      idAttr := "current-post-card",
      paddingLeft.rem(Client.CardPaddingLeftRightRem),
      paddingRight.rem(Client.CardPaddingLeftRightRem),
      display.flex,
      //height.percent(100),
      div(
        idAttr := "current-post-card-no-post",
        display <-- currentPostDefinitionSignal.map( opt => if opt.isEmpty then "block" else "none" ),
        NoPostChosenLabel
      ),
      div(
        idAttr := "current-post-card-with-post",
        display.flex,
        flexDirection.column,
        flexGrow(1),
        //height.percent(100),
        display <-- currentPostDefinitionSignal.map( opt => if opt.isEmpty then "none" else "flex" ),
        div(
          idAttr := "current-post-card-title",
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
          idAttr := "current-post-card-author",
          marginLeft.em(2),
          "by Alice Aarvark and Bob Barnyard"
        ),
        div(
          idAttr := "current-post-card-compose",
          display.flex,
          flexGrow(1),
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

