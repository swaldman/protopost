package protopost.client

import protopost.api.{DestinationIdentifier,PosterNoAuth,PostDefinition,PostIdentifier,given}

import util.laminar.onEnterPress

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object CurrentPostCard:
  def create(
    destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
    currentPostIdentifierVar : Var[Option[PostIdentifier]],
    posterNoAuthSignal : Signal[Option[PosterNoAuth]]
  ) : HtmlElement =
    val currentPostIdentifierSignal = currentPostIdentifierVar.signal
    val currentPostDefinitionSignal = currentPostIdentifierSignal.withCurrentValueOf(destinationsToKnownPostsVar).map: (mbpi,d2kp) =>
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

    val titleChangeEventBus = new EventBus[HtmlElement]
    val titleChangeEventStreamWithCurrentPost = titleChangeEventBus.events.withCurrentValueOf(currentPostDefinitionSignal)

    val titleChangeObserver = Observer[(HtmlElement, Option[PostDefinition])]: ( node, mbPd ) =>
      mbPd match
        case Some(pd) =>
          val newText = node.ref.textContent.trim
          val currentOpt = pd.title
          if Some(newText) != currentOpt then
            dom.window.alert(s"update title to ${newText}")
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
            text <-- currentPostDefinitionSignal.map( _.map( _.title.getOrElse("(untitled post)") ).getOrElse("") ),
            inContext { thisNode =>
              onEnterPress.preventDefault.mapTo(thisNode) --> titleChangeEventBus
            },
            inContext { thisNode =>
              onBlur.mapTo(thisNode) --> titleChangeEventBus
            }
          ),
          " ",
          span(
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

