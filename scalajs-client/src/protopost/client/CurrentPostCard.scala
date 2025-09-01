package protopost.client

import protopost.api.{DestinationIdentifier,PosterNoAuth,PostDefinition,PostDefinitionUpdate,PostIdentifier,given}

import util.laminar.onEnterPress

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.api.{PostDefinitionCreate,UpdateValue}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object CurrentPostCard:
  def create(
    protopostLocation : Uri,
    backend : WebSocketBackend[scala.concurrent.Future],
    destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
    currentPostIdentifierVar : Var[Option[PostIdentifier]],
    posterNoAuthSignal : Signal[Option[PosterNoAuth]]
  ) : HtmlElement =
    val currentPostIdentifierSignal = currentPostIdentifierVar.signal
    val currentPostDefinitionSignal = Signal.combine(currentPostIdentifierSignal,destinationsToKnownPostsVar).map: (mbpi,d2kp) =>
      mbpi.flatMap: pi =>
        val mbDestinationMap = d2kp.get(pi.destinationIdentifier)
        mbDestinationMap.flatMap( dm => dm.get(pi.postId) )
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

    div(
      idAttr := "current-post-card",
      paddingLeft.rem(Client.CardPaddingLeftRightRem),
      paddingRight.rem(Client.CardPaddingLeftRightRem),
      div(
        idAttr := "current-post-card-no-post",
        display <-- currentPostIdentifierSignal.map( opt => if opt.isEmpty then "block" else "none" ),
        "No post chosen"
      ),
      div(
        idAttr := "current-post-card-with-post",
        display <-- currentPostIdentifierSignal.map( opt => if opt.isEmpty then "none" else "block" ),
        div(
          span(
            fontSize.pt(Client.CardTitleFontSizePt),
            fontWeight.bold,
            contentEditable <-- userIsOwnerSignal,
            backgroundColor <-- titleBackgroundColorSignal,
            text <-- currentPostDefinitionSignal.map( _.map( _.title.getOrElse("(untitled post)") ).getOrElse("") ),
            inContext { thisNode =>
              //onEnterPress.preventDefault.mapTo(thisNode) --> titleChangeEventBus
              onEnterPress.preventDefault --> { whatever =>
                thisNode.ref.blur()
              }
            },
            inContext { thisNode =>
              onBlur.mapTo(thisNode) --> titleChangeEventBus
            }
          ),
          " ",
          span(
            color.gray,
            fontStyle.italic,
            display <-- userIsOwnerSignal.map( if _ then "inline" else "none" ),
            "\u2190 edit the title here"
          )
        )
      ),
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        given Owner = mountContext.owner
        titleChangeEventStreamWithCurrentPost.addObserver( titleChangeObserver )
      },
    )

