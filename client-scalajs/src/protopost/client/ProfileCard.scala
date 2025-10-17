package protopost.client

import protopost.common.api.{PosterNoAuth, given}

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}
import protopost.common.EmailAddress

object ProfileCard:
  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

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
      hr(
      ),
      div(
        label(
          forId := "profile-composer-properties",
          PublishDetailsPaneLabelCommonModifiers,
          "composer properties"
        ),
        div(
          sectionBorderPaddingMargin,
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
      )
    )

