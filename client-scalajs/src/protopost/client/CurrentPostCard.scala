package protopost.client

import protopost.common.api.{DestinationIdentifier,PosterNoAuth,PostDefinition,PostDefinitionUpdate,PostIdentifier,given}

import util.laminar.{documentEscapeEvents,onEnterPress}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.common.api.{PostDefinitionCreate,PostRevisionHistory,UpdateValue}

import com.mchange.conveniences.string.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object CurrentPostCard:

  private val NoPostChosenLabel = "No post chosen"

  def create( client : Client ) : HtmlElement =
    import client.*

    val userIsCurrentPostOwnerSignal : Signal[Boolean] = Signal.combine(posterNoAuthSignal,currentPostDefinitionSignal).map: (mbPna, mbCpd) =>
      val mbOut =
        for
          pna <- mbPna
          cpd <- mbCpd
        yield
          pna.id == cpd.owner.id
      //println(s"(mbPna, mbCpd, mbOut) ($mbPna, $mbCpd, $mbOut)")
      mbOut.getOrElse(false)

    val currentAuthorsSignal : Signal[Option[Seq[String]]] = currentPostDefinitionSignal.map: mbpd =>
      mbpd.map: pd =>
        pd.authors

    val currentPostTitleDirtyVar = Var(false)
    val currentPostTitleBackgroundColorSignal = currentPostTitleDirtyVar.signal.map( if _ then "yellow" else "white" )

    val currentPostTitleChangeEventBus = new EventBus[HtmlElement]
    val currentPostTitleChangeEventStreamWithCurrentPost = currentPostTitleChangeEventBus.events.withCurrentValueOf(currentPostDefinitionSignal)

    val currentPostAuthorDirtyVar = Var(false)
    val currentPostAuthorBackgroundColorSignal = currentPostAuthorDirtyVar.signal.map( if _ then "yellow" else "white" )

    val currentPostAuthorChangeEventBus = new EventBus[HtmlElement]
    val currentPostAuthorChangeEventStreamWithCurrentPost = currentPostAuthorChangeEventBus.events.withCurrentValueOf(currentPostDefinitionSignal)

    def detailsChangeObserver[T](
      itemFromTextContent : String => Option[T],
      itemFromPostDefinition : PostDefinition => Option[T],
      rewrite : (PostDefinition, Option[T]) => PostDefinitionUpdate,
      dirtyVar : Var[Boolean]
    ) : Observer[(HtmlElement, Option[PostDefinition])] =
      Observer[(HtmlElement, Option[PostDefinition])]: ( node, mbPd ) =>
        //println(s"( node, mbPd ): ( ${node}, $mbPd )   ${java.time.Instant.now()}")
        mbPd match
          case Some(pd) =>
            val newTextOpt = itemFromTextContent( node.ref.textContent )
            val currentOpt = itemFromPostDefinition(pd)
            if newTextOpt != currentOpt then
              val pdu = rewrite(pd, newTextOpt)
              dirtyVar.set(true)
              util.request.hardUpdatePostDefinitionUpdate(protopostLocation,pd.destination.destinationIdentifier, pdu, backend, destinationsToKnownPostsVar, _ => dirtyVar.set(false))
          case None =>
            println("No post definition to update. Try to prevent any capacity to edit in this case.")
        node.ref.blur()

    val titleChangeObserver = detailsChangeObserver(
      _.toOptionNotBlank,
      _.title,
      (pd,nto) => PostDefinitionUpdate( pd.postId, title = nto.fold( UpdateValue.`set-to-none` )( UpdateValue.`update`.apply ) ),
      currentPostTitleDirtyVar
    )

    val authorChangeObserver =
      def itemFromTextContent( s : String ) : Option[Seq[String]] =
        val trimmed = s.trim
        val unprefixed = if trimmed.startsWith("by") && Character.isWhitespace(trimmed.charAt(2)) then trimmed.substring(3).trim else trimmed
        Some(parseCommaListAnd(unprefixed))
      def itemFromPostDefinition( pd : PostDefinition ) : Option[Seq[String]] = Some( pd.authors )
      def rewrite(pd : PostDefinition, nso : Option[Seq[String]]) : PostDefinitionUpdate = PostDefinitionUpdate( pd.postId, authors = nso.fold( UpdateValue.`set-to-none` )( UpdateValue.`update`.apply ) )
      detailsChangeObserver( itemFromTextContent, itemFromPostDefinition, rewrite, currentPostAuthorDirtyVar )

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

    lazy val composerPanePreview = ComposerPane.create( client )
    lazy val composerPaneWysiwyg = CkEditorComposerPane.create( client )

    val composerPaneSignal = composerSignal.map: composer =>
      composer match
        case Composer.`text-and-preview` => composerPanePreview
        case Composer.WYSIWYG            => composerPaneWysiwyg

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
            contentEditable <-- userIsCurrentPostOwnerSignal,
            backgroundColor <-- currentPostTitleBackgroundColorSignal,
            text <-- currentPostDefinitionSignal.map( _.map( _.title.getOrElse(Client.UntitledPostLabel) ).getOrElse("") ),
            inContext { thisNode =>
              //onEnterPress.preventDefault.mapTo(thisNode) --> titleChangeEventBus
              onEnterPress.preventDefault --> { whatever =>
                thisNode.ref.blur()
              }
            },
            inContext { thisNode =>
              onBlur.mapTo(thisNode) --> currentPostTitleChangeEventBus
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
            display <-- userIsCurrentPostOwnerSignal.map( if _ then "inline" else "none" ),
            "\u2190 edit the title here"
          )
        ),
        div(
          idAttr := "current-post-card-author",
          lineHeight.percent(120),
          marginLeft.em(1),
          contentEditable <-- userIsCurrentPostOwnerSignal,
          backgroundColor <-- currentPostAuthorBackgroundColorSignal,
          text <-- currentAuthorsSignal.map( mbAuthors => mbAuthors.fold("")(authors => commaListAnd(authors).fold("")("by " + _) ) ),
          inContext { thisNode =>
            //onEnterPress.preventDefault.mapTo(thisNode) --> titleChangeEventBus
            onEnterPress.preventDefault --> { whatever =>
              thisNode.ref.blur()
            }
          },
          inContext { thisNode =>
            onBlur.mapTo(thisNode) --> currentPostAuthorChangeEventBus
          },
          inContext { thisNode =>
            documentEscapeEvents.compose( _.withCurrentValueOf(currentPostDefinitionSignal) ) --> { (_,mbCpd) =>
              if thisNode.ref == dom.document.activeElement then
                thisNode.ref.textContent = mbCpd.fold( "" )( cpd => commaListAnd( cpd.authors ).fold("")( "by " + _ ) )
                thisNode.ref.blur()
            }
          }
        ),
        div(
          idAttr := "current-post-card-compose",
          display.flex,
          flexGrow(1),
          child <-- composerPaneSignal
        )
      ),
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        given Owner = mountContext.owner
        currentPostTitleChangeEventStreamWithCurrentPost.addObserver( titleChangeObserver )
        currentPostAuthorChangeEventStreamWithCurrentPost.addObserver( authorChangeObserver )
        currentPostIdentifierSignal.withCurrentValueOf(destinationsToKnownPostsVar).addObserver( postIdentifierObserver )
      },
    )

