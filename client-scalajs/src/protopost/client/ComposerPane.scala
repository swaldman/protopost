package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object ComposerPane:
  enum Tab:
    case edit, preview

  val serifFontFamilies = "Georgia, Garamond, serif"

  val contentTypeSelect = "composer-content-type-select"

  def statusCircle() : HtmlElement =
    div(
      backgroundColor.yellow,
      borderRadius.percent(50),
      width.rem(0.75),
      height.rem(0.75),
      borderColor.black,
      borderWidth.px(1),
      borderStyle.solid
    )

  def create(
    currentPostLocalPostContentLsi : LocalStorageItem[PostContent],
    localContentDirtyVar           : Var[Boolean],
    manualSaveWriteBus : WriteBus[Unit]
  ) : HtmlElement =
    val currentPostLocalPostContentSignal = currentPostLocalPostContentLsi.signal
    val localContentDirtySignal = localContentDirtyVar.signal

    val contentTypeChangeObserver = Observer[String]: (value) =>
      currentPostLocalPostContentLsi.update( _.copy(contentType=value) )
      localContentDirtyVar.set(true)

    val textAreaChangeObserver = Observer[String]: (value) =>
      currentPostLocalPostContentLsi.update( _.copy(text=value) )
      localContentDirtyVar.set(true)

    val currentTabVar : Var[ComposerPane.Tab] = Var(ComposerPane.Tab.edit)
    val currentTabSignal = currentTabVar.signal

    val previewPane = div("This is the preview pane")

    def tabsDiv() =
      val tabs =
        Tab.values.map: tab =>
          div(
            cls := "tab-pane",
            flexGrow(1),
            textAlign.center,
            cls <-- currentTabSignal.map( currentTab => if tab == currentTab then "current" else "" ),
            TinyLink.create(tab.toString).amend(
              onClick --> ( click => currentTabVar.set(tab) ),
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
        styleTag(
          """
          |#composer-text-area:focus {
          |  border-color: black;
          |  outline: none;
          |}
          """.stripMargin
        ),
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
          onInput.mapToValue.compose( _.debounce(500) ) --> textAreaChangeObserver,
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
        text <-- currentTabSignal.withCurrentValueOf(currentPostLocalPostContentSignal).map( (_,pc) => pc.text )
      )

    val composeCardPreviewTextHtml =
      div(
        previewModifiersCommon,
        fontFamily(serifFontFamilies),
        inContext { thisNode =>
          currentTabSignal.withCurrentValueOf(currentPostLocalPostContentSignal) --> { (tab,pc) => 
            thisNode.ref.innerHTML = DOMPurify.sanitize(pc.text)
          }
        }
      )

    val composeCardPreviewTextMarkdown =
      div(
        previewModifiersCommon,
        fontFamily(serifFontFamilies),
        inContext { thisNode =>
          currentTabSignal.withCurrentValueOf(currentPostLocalPostContentSignal) --> { (tab,pc) => 
            thisNode.ref.innerHTML = marked.parse( DOMPurify.sanitize(pc.text) )
          }
        }
      )

    val composeCardSignal =
      Signal.combine(currentTabSignal,currentPostLocalPostContentSignal).map: ( tab, pc ) =>
        tab match
          case ComposerPane.Tab.edit => composeCardEdit
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
            statusCircle().amend(
              backgroundColor <-- localContentDirtySignal.map( dirty => if dirty then "yellow" else "#22ff22" )
            ),
          )
        ),
        div(
          idAttr := "compose-review-details-switch-panel",
          //maxHeight.percent(100),
          flexGrow(1),
          display.flex,
          flexDirection.column,
          child <-- composeCardSignal
        )
      )
    )
