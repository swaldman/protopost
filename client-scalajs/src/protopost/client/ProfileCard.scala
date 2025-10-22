package protopost.client

import protopost.common.api.{PosterNoAuth, given}

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.common.EmailAddress
import protopost.common.api.{NewPostRevision,RevisionTimestamp}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*


object ProfileCard:
  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

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
    )

