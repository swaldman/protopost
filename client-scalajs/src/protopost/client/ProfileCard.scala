package protopost.client

import protopost.common.api.{PosterNoAuth, given}

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.common.EmailAddress
import protopost.common.api.RevisionTimestamp
import protopost.common.api.NewPostRevision

object ProfileCard:
  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

    val selectedRecoveredRevisionVar : Var[Option[RevisionTimestamp]] = Var(None)
    val selectedRecoveredRevisionSignal = selectedRecoveredRevisionVar.signal

    val recoveredRevisionsTableHeadeModifiers = Seq(
      fontWeight.bold,
      fontSize.pt(10),
      textDecoration.underline,
      marginBottom.rem(0.25),
      textAlign.center,
    )

    val commonRevisionsRowModifiers = Seq(
      fontSize.pt(10),
      textAlign.center,
      // borderColor.fuchsia,
      // borderWidth.px(1),
      // borderStyle.solid,
    )

    val recoveredRevisionsRowSignal : Signal[Seq[Seq[HtmlElement]]] =
      def makeRow( id : RevisionTimestamp, initial : Tuple2[RevisionTimestamp,NewPostRevision], updates : Signal[Tuple2[RevisionTimestamp,NewPostRevision]] ) : Seq[HtmlElement] =
        Seq(
          div(
            commonRevisionsRowModifiers,
            initial(1).postId.toString()
          ),
          div(
            commonRevisionsRowModifiers,
            TinyLink.create( Client.RevisionTimestampFormatter.format(id.asInstant) ).amend(
              onClick --> { _ => selectedRecoveredRevisionVar.set(Some(id)) }
            )
          ),
          div(
            commonRevisionsRowModifiers,
            cursor.pointer,
            alignSelf.center,
            color.red,
            marginLeft.rem(0.25),
            "\u00d7",
            // onClick --> { _ =>
            //   util.request.deleteMediaItemForPost(protopostLocation,postId,path,backend,currentPostMediaVar)
            // }
          )
        )
      recoveredRevisionsSignal.split( _(0) )( makeRow )

    val recoveredRevisionTableCard =
      div(
        idAttr := "profile-recovered-revisions-table-card",
        display.grid,
        styleProp("grid-template-columns") := "max-content max-content max-content",
        marginLeft.auto,
        marginRight.auto,
        columnGap.rem(0.5),
        div(
          recoveredRevisionsTableHeadeModifiers,
          "post"
        ),
        div(
          recoveredRevisionsTableHeadeModifiers,
          "revision"
        ),
        div(
          "\u00a0" // non-breaking space
        ),
        children <-- recoveredRevisionsRowSignal.map( _.flatten )
      )

    val recoveredRevisionPreviewCard =
      div(
        "Test."
      )

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
          "composer properties"
        ),
        div(
          sectionBorderPaddingMargin,
          idAttr := "profile-composer-properties",
          display.flex,
          flexDirection.column,
          //fontSize.pt(Client.CardBaseTextSizePt),
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
          "recovered unsaved revisions"
        ),
        div(
          idAttr := "profile-recovered-revisions",
          display.flex,
          justifyContent.center,
          alignContent.center,
          sectionBorderPaddingMargin,
          recoveredRevisionTableCard.amend(
            display <-- selectedRecoveredRevisionSignal.map( mbrr => if mbrr.isEmpty then "grid" else "none" )
          ),
          recoveredRevisionPreviewCard.amend(
            display <-- selectedRecoveredRevisionSignal.map( mbrr => if mbrr.isEmpty then "none" else "block" )
          ),
        ),
      ),
    )

