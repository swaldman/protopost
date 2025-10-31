package protopost.client

import protopost.common.api.{PosterNoAuth, given}

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.common.EmailAddress
import protopost.common.api.{Destination,DestinationIdentifier,NewPostRevision,RevisionTimestamp,SubscribableFeed}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import scala.collection.immutable

import util.laminar.onEnterPress
import javax.swing.border.Border


object ProfileCard:
  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

    val destinationToFeedsVar : Var[immutable.SortedMap[Destination,immutable.SortedSet[SubscribableFeed]]] = Var(immutable.SortedMap.empty)
    val destinationToFeedsSignal = destinationToFeedsVar.signal

    val destinationsObserver = Observer[immutable.SortedSet[Destination]]: destinations =>
      destinations.foreach: destination =>
        util.request.updateFeedsForDestination( protopostLocation, destination, backend, destinationToFeedsVar )

    val recoveredRevisionsTableHeaderModifiers = Seq(
      fontWeight.bold,
      fontSize.pt(10),
      //textDecoration.underline,
      marginBottom.rem(0.25),
    )

    val commonRevisionsRowModifiers = Seq(
      fontSize.pt(10),
      // borderColor.fuchsia,
      // borderWidth.px(1),
      // borderStyle.solid,
    )

    val recoveredRevisionsRowSignal : Signal[Seq[Seq[HtmlElement]]] =
      def makeRow( id : RevisionTimestamp, initial : UnsavedRevision, updates : Signal[UnsavedRevision] ) : Seq[HtmlElement] =
        Seq(
          div(
            commonRevisionsRowModifiers,
            textAlign.center,
            initial.newPostRevision.postId.toString(),
          ),
          div(
            //borderColor.fuchsia,
            //borderWidth.px(1),
            //borderStyle.solid,
            commonRevisionsRowModifiers,
            textAlign.left,
            util.destinationText( initial.destination )
          ),
          div(
            commonRevisionsRowModifiers,
            textAlign.left,
            TinyLink.create( Client.RevisionTimestampFormatter.format(id.asInstant) ).amend(
              onClick( _.withCurrentValueOf(updates) ) --> { (_, ur) => selectedUnsavedRevisionVar.set(Some(ur)) }
            )
          ),
          div(
            commonRevisionsRowModifiers,
            cursor.pointer,
            alignSelf.center,
            color.red,
            marginLeft.rem(0.25),
            "\u00d7",
            onClick --> { _ =>
              recoveredRevisionsLsi.update( _.filter( _.revisionTimestamp != id ) )
            }
          )
        )
      recoveredRevisionsSignal.split( _.revisionTimestamp )( makeRow )

    val noRecoveredRevisionsCard =
      div(
        fontSize.pt(10),
        fontStyle.italic,
        "No unsaved revisions are available."
      )

    val recoveredRevisionTableCard =
      div(
        // borderWidth.px(1),
        // borderStyle.solid,
        // borderColor.fuchsia,
        display.flex,
        justifyContent.center,
        div(
          maxWidth.fitContent,
          // borderWidth.px(1),
          // borderStyle.solid,
          // borderColor.green,
          idAttr := "profile-recovered-revisions-table-card",
          display.grid,
          styleProp("grid-template-columns") := "max-content max-content max-content max-content",
          columnGap.rem(0.5),
          div(
            recoveredRevisionsTableHeaderModifiers,
            textAlign.center,
            "post"
          ),
          div(
            recoveredRevisionsTableHeaderModifiers,
            textAlign.left,
            "destination"
          ),
          div(
            recoveredRevisionsTableHeaderModifiers,
            textAlign.left,
            "timestamp"
          ),
          div(
            "\u00a0" // non-breaking space
          ),
          children <-- recoveredRevisionsRowSignal.map( _.flatten )
        )
      )

    val recoveredRevisionPreviewCard = UnsavedRevisionPreviewCard.create( client )

    def unselectedCard() : HtmlElement =
      if recoveredRevisionsLsi.now().nonEmpty then recoveredRevisionTableCard else noRecoveredRevisionsCard


    val destinationRssCardsSignal =
      def makeRssFeedRow( destination : Destination )( feedId : Int, initial : SubscribableFeed, updates : Signal[SubscribableFeed] ) : Seq[HtmlElement] =
        Seq(
          div(
            text <-- updates.map( _.title )
          ),
          div(
            cursor.pointer,
            alignSelf.center,
            color.red,
            marginLeft.rem(0.5),
            "\u00d7",
            onClick --> { _ =>
              util.request.unsubscribeDestinationFromFeed( protopostLocation, destination, feedId, backend, destinationToFeedsVar )
            }
          )
        )
      def makeDestinationPane( id : DestinationIdentifier, initial : (Destination, immutable.SortedSet[SubscribableFeed]), updates : Signal[(Destination, immutable.SortedSet[SubscribableFeed])] ) : HtmlElement =

        val messageVar : Var[Option[String]] = Var(None)
        val resetBus = EventBus[Unit]()

        val clearMessageObserver = Observer[Any]( _ => messageVar.set(None) )

        val destination = initial(0)

        lazy val noSubscritionsPane =
          div(
            //fontSize.pt(10),
            fontWeight.normal,
            em( "No subscriptions." )
          )

        div(
          display.flex,
          flexDirection.column,
          borderColor := "#ddd",
          borderWidth.px(1),
          borderStyle.solid,
          margin.rem(0.25),
          padding.rem(0.25),
          fontSize.pt(10),
          div(
            fontWeight.bold,
            marginTop.rem(0.5),
            marginBottom.rem(0.25),
            util.destinationText( destination )
          ),
          div(
            marginLeft.rem(1),
            display.grid,
            styleProp("grid-template-columns") := "max-content max-content",
            children <-- updates.map( _(1) ).split( _.feedId )( makeRssFeedRow(destination) ).map( _.toSeq.flatten ).map( s => if s.isEmpty then Seq(noSubscritionsPane) else s ),
          ),
          div(
            display.flex,
            flexDirection.column,
            marginTop.rem(0.25),
            marginBottom.rem(0.25),
            div(
              display.flex,
              flexDirection.row,
              alignItems.center,
              fontSize.pt(9),
              label(
                forId := s"rss-subscribe-textfield-${id.seismicNodeId}-${id.name}",
                fontWeight.bold,
                paddingRight.rem(0.5),
                "add feeds (html, rss, atom):"
              ),
              input(
                idAttr := s"rss-subscribe-textfield-${id.seismicNodeId}-${id.name}",
                flexGrow(1),
                `type` := "text",
                value <-- resetBus.events.map( _ => "" ),
                onEnterPress.mapToValue --> { feedSource =>
                  //println( s"$id -- subscribing to feed '${feedSource}'" )
                  util.request.subscribeDestinationToFeedsFromFeedSource(
                    protopostLocation,
                    destination,
                    feedSource,
                    backend,
                    messageVar,
                    destinationToFeedsVar
                  )
                  resetBus.writer.onNext( () )
                },
                onBlur   --> clearMessageObserver,
                onChange --> clearMessageObserver,
                onKeyDown --> clearMessageObserver,
              )
            )
          ),
          div(
            fontSize.pt(9),
            color.red,
            //borderStyle.solid, borderWidth.px(2), borderColor.red,
            idAttr := s"rss-subscribe-textfield-message-${id.seismicNodeId}-${id.name}",
            text <-- messageVar.signal.map( _.getOrElse("\u00a0") )
          )
        )
      destinationToFeedsSignal.map( _.toList ).split(tup => tup(0).destinationIdentifier)( makeDestinationPane )

    def makeComposerRadioButton( composer : Composer ) : HtmlElement =
      div(
        input(
          `type`   := "radio",
          nameAttr := "composer",
          value    := composer.toString(),
          checked <-- composerSignal.map( _ == composer ),
          onInput.mapTo(composer) --> ( c => composerLsi.set(c) ),
        ),
        composer.label
      )

    div(
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        given Owner = mountContext.owner
        destinationsSignal.addObserver( destinationsObserver )
      },
      idAttr("profile-panel"),
      //backgroundColor("#ccccff"),
      //boxSizing.borderBox,
      width.percent(100),
      height.percent(100),
      paddingLeft.rem(Client.CardPaddingLeftRightRem),
      paddingRight.rem(Client.CardPaddingLeftRightRem),
      borderWidth.px(3),
      borderColor.black,
      // margin.rem(1.5),
      // display.flex,
      // flexDirection.column,
      div(
        idAttr := "profile-name-pane",
        fontSize.pt(Client.CardTitleFontSizePt),
        fontWeight.bold,
        text <-- posterNoAuthSignal.map: mbPna =>
          mbPna match
            case Some( pna ) => s"Hello, ${pna.fullName}!"
            case None => ""
      ),
      util.laminar.blackHr(),
      div(
        idAttr := "profile-info-pane",
        marginLeft.rem(1),
        fontSize.pt(Client.CardBaseTextSizePt),
        div(
          b("ID "),
          text <-- posterNoAuthSignal.map: mbPna =>
            mbPna match
              case Some( pna ) => s"#${pna.id}"
              case None => ""
        ),
        div(
          b("email: "),
          //contentEditable(true),
          text <-- posterNoAuthSignal.map: mbPna =>
            mbPna match
              case Some( pna ) => EmailAddress.s(pna.email)
              case None => ""
        ),
      ),
      util.laminar.blackHr(),
      div(
        marginTop.rem(1),
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "composer"
        ),
        div(
          sectionBorderPaddingMargin,
          idAttr := "profile-composer-properties",
          display.flex,
          flexDirection.column,
          fontSize.pt(10),
          fontWeight.normal,
          lineHeight.percent(150),
          makeComposerRadioButton( Composer.`WYSIWYG`),
          div(
            display.flex,
            flexDirection.row,
            marginLeft.rem(1.5),
            input(
              `type` := "checkbox",
              disabled <-- composerSignal.map( _ != Composer.`WYSIWYG` ),
              checked <-- externalJsConfigSignal.map( _.ckeditorLoadedImagesDefaultToWidth100Percent ),
              onInput.mapToChecked --> { checked =>
                externalJsConfigManager.update( _.copy(ckeditorLoadedImagesDefaultToWidth100Percent = checked) )
              }
            ),
            div (
              marginLeft.rem(0.25),
              "image insertions default to width 100%"
            )
          ),
          makeComposerRadioButton( Composer.`text-and-preview`),
        )
      ),
      div(
        marginTop.rem(1),
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "unsaved recovered revisions"
        ),
        div(
          idAttr := "profile-recovered-revisions",
          display.flex,
          flexDirection.column,
          justifyContent.center,
          alignContent.stretch,
          sectionBorderPaddingMargin,
          child <-- Signal.combine(selectedUnsavedRevisionSignal,recoveredRevisionsSignal).map( tup => tup(0).fold( unselectedCard() )( _ => recoveredRevisionPreviewCard ) ),
        ),
      ),
      div(
        marginTop.rem(1),
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "feeds to scan for comments"
        ),
        div(
          idAttr := "profile-rss-subscribed",
          display.flex,
          flexDirection.column,
          // justifyContent.center,
          alignContent.stretch,
          sectionBorderPaddingMargin,
          padding.rem(0.25), // override!
          children <-- destinationRssCardsSignal,
        ),
      ),
    )
