package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import protopost.common.api.{PostDefinition,PostRevisionHistory}
import protopost.client.util.safeHtmlFromMarkdown
import protopost.client.util.safeHtmlFromUserHtml

object ComposerPane:
  enum Tab:
    case edit, preview, publish

  val contentTypeSelect = "composer-content-type-select"

  def create( client : Client ) : HtmlElement =
    import client.*

    val currentPostLocalPostContentSignal = currentPostLocalPostContentLsi.signal

    val contentTypeChangeObserver = Observer[String]: (value) =>
      currentPostLocalPostContentLsi.update( _.copy(contentType=value) )
      localContentDirtyVar.set(true)

    val currentPostDefinitionChangeObserver = Observer[Option[PostDefinition]]: mbpd =>
      composerPaneCurrentTabVar.set(ComposerPane.Tab.edit) // go back to default edit tab when the post definition has updates

    val previewPane = div("This is the preview pane")

    def tabsDiv() =
      val tabs =
        Tab.values.map: tab =>
          div(
            cls := "tab-pane",
            flexGrow(1),
            textAlign.center,
            cls <-- composerPaneCurrentTabSignal.map( currentTab => if tab == currentTab then "current" else "" ),
            TinyLink.create(tab.toString).amend(
              onClick --> ( click => composerPaneCurrentTabVar.set(tab) ),
            )
          )
      div(
        idAttr := "composer-pane-tabs-div",
        display.flex,
        flexDirection.row,
        alignItems.center,
        justifyContent.center,
        flexGrow(1),
        tabs
      )

    val composeCardEdit =
      div(
        flexGrow(1),
        display.flex,
        textArea(
          idAttr := "composer-text-area",
          fontFamily("monospace"),
          padding.rem(1),
          borderStyle.solid,
          borderColor.black,
          borderWidth.px(2),
          borderRadius.px(10),
          flexGrow(1),
          //width.percent(100),
          //height.percent(100),
          resize("none"),
          onInput.mapToValue.compose( _.debounce(500) ) --> composerRawTextAreaChangeObserver,
          value <-- currentPostLocalPostContentSignal.map( _.text )
        )
      )

    val previewModifiersCommon =
      Seq(
        //width.percent(100),
        //height.percent("calc(100% - 2rem)"),
        flexGrow(1),
        minHeight.px(0),
        height.px(0),
        //maxHeight.calc("100% - 5rem"),
        overflowY.scroll,
        borderStyle.solid,
        borderWidth.px(1),
        borderColor.gray,
        backgroundColor("#EEEEEE"),
        marginTop.rem(0),
        marginBottom.rem(0),
        marginLeft.rem(0),
        marginRight.rem(0),
        padding.rem(1),
      )

    val composeCardPreviewTextPlain =
      div(
        previewModifiersCommon,
        whiteSpace.pre,
        fontFamily("monospace"),
        text <-- composerPaneCurrentTabSignal.withCurrentValueOf(currentPostLocalPostContentSignal).map( (_,pc) => pc.text )
      )

    val composeCardPreviewTextHtml =
      div(
        previewModifiersCommon,
        fontFamily(serifFontFamilies),
        inContext { thisNode =>
          composerPaneCurrentTabSignal.withCurrentValueOf(currentPostLocalPostContentSignal) --> { (tab,pc) => 
            thisNode.ref.innerHTML = safeHtmlFromUserHtml( pc.text )
          }
        }
      )

    val composeCardPreviewTextMarkdown =
      div(
        previewModifiersCommon,
        fontFamily(serifFontFamilies),
        inContext { thisNode =>
          composerPaneCurrentTabSignal.withCurrentValueOf(currentPostLocalPostContentSignal) --> { (tab,pc) => 
            thisNode.ref.innerHTML = safeHtmlFromMarkdown( pc.text )
          }
        }
      )

    val composeCardPublishDetailsPane =
      div(
        flexGrow(1),
        PublishDetailsPane.create( client )
      )

    val composeCardReloginPane =
      div(
        flexGrow(1),
        LoginForm.create( client )
      )

    val composeCardSignal =
      Signal.combine(composerPaneCurrentTabSignal,currentPostLocalPostContentSignal,loginLevelSignal).map: ( tab, pc, ll ) =>
        tab match
          case ComposerPane.Tab.edit    => composeCardEdit
          case ComposerPane.Tab.publish =>
            if ll == LoginLevel.high then
              composeCardPublishDetailsPane
            else
              composeCardReloginPane
          case ComposerPane.Tab.preview =>
            pc.contentType match
              case "text/plain"     => composeCardPreviewTextPlain
              case "text/html"      => composeCardPreviewTextHtml
              case "text/markdown"  => composeCardPreviewTextMarkdown

    div(
      idAttr := "composer-pane",
      // backgroundColor("lightGray"),
      //height.calc("100% - 3rem"),
      //maxHeight.calc("100% - 3rem"),
      flexGrow(1),
      overflowY := "clip",
      marginTop.rem(1),
      marginLeft.rem(1),
      marginRight.rem(1),
      display.flex,
      div(
        idAttr := "compose-text-and-preview-card",
        flexGrow(1),
        display.flex,
        flexDirection.column,
        //height.percent(100),
        //maxHeight.percent(100),
        div(
          marginBottom.rem(0.5),
          idAttr := "compose-text-and-preview-toolbar",
          display.flex,
          flexDirection.row,
          alignItems.center,
          justifyContent.center,
          div(
            idAttr := "compose-text-and-preview-content-type-select-pane",
            label(
              forId := contentTypeSelect,
              fontSize.pt(11),
              fontWeight.bold,
              "type: "
            ),
            select(
              idAttr := contentTypeSelect,
              nameAttr := contentTypeSelect,
              option (
                value := "text/plain",
                "text/plain"
              ),
              option (
                value := "text/html",
                "text/html"
              ),
              option (
                value := "text/markdown",
                "text/markdown"
              ),
              onChange.mapToValue --> contentTypeChangeObserver,
              value <-- currentPostLocalPostContentSignal.map( _.contentType )
            )
          ),
          tabsDiv(),
          button(
            cls:= "button-utilitarian",
            role("button"),
            disabled <-- localContentDirtySignal.map(!_),
            onClick.mapToUnit --> manualSaveWriteBus,
            "save draft",
          ),
          div(
            paddingLeft.rem(0.5),
            localContentStatusCircle(),
          ),
        ),
        div(
          idAttr := "compose-review-details-switch-panel",
          //maxHeight.percent(100),
          flexGrow(1),
          display.flex,
          flexDirection.column,
          child <-- composeCardSignal
        )
      ),
      onMountCallback { mountContext =>
        given Owner = mountContext.owner
        currentPostDefinitionChangeEvents.addObserver(currentPostDefinitionChangeObserver)
      },
    )
