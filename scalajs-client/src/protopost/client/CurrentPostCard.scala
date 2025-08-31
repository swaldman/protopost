package protopost.client

import protopost.api.{DestinationIdentifier,PostDefinition,PostIdentifier,given}

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object CurrentPostCard:
  def create( destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]], currentPostIdentifierVar : Var[Option[PostIdentifier]] ) : HtmlElement =
    val currentPostIdentifierSignal = currentPostIdentifierVar.signal
    val currentPostDefinitionSignal = currentPostIdentifierSignal.withCurrentValueOf(destinationsToKnownPostsVar).map: (mbpi,d2kp) =>
      mbpi.flatMap: pi =>
        val mbDestinationMap = d2kp.get(pi.destinationIdentifier)
        mbDestinationMap.flatMap( dm => dm.get(pi.postId) )

    div(
      idAttr := "current-post-card",
      paddingLeft.rem(Client.CardPaddingLeftRightRem),
      paddingRight.rem(Client.CardPaddingLeftRightRem),
      div(
        idAttr := "current-post-card-no-post-pane",
        display <-- currentPostIdentifierSignal.map( opt => if opt.isEmpty then "block" else "none" ),
        "No post chosen"
      ),
      div(
        idAttr := "current-post-card-with-post-pane",
        display <-- currentPostIdentifierSignal.map( opt => if opt.isEmpty then "none" else "block" ),
        div(
          fontSize.pt(Client.CardTitleFontSizePt),
          fontWeight.bold,
          contentEditable(true),
          text <-- currentPostDefinitionSignal.map( _.map( _.title.getOrElse("(untitled post)") ).getOrElse("") ),
          inContext { thisNode =>
            util.laminar.onEnterPress.preventDefault.mapTo(thisNode) --> { node =>
              dom.window.alert(node.ref.textContent)
              node.ref.blur()
            }
          }
        )
      ),
    )

