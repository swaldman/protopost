package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object ComposerPane:
  val contentTypeSelect = "composer-content-type-select"

  def statusCircle() : HtmlElement =
    div(
      backgroundColor.red,
      borderRadius.percent(50),
      width.rem(0.75),
      height.rem(0.75),
      borderColor.black,
      borderWidth.px(1),
      borderStyle.solid
    )

  def create(
    currentPostLocalContentTypeLsi : LocalStorageItem[String],
    currentPostLocalTextLsi : LocalStorageItem[String]
  ) : HtmlElement =
    val currentPostLocalContentTypeSignal = currentPostLocalContentTypeLsi.signal
    val currentPostLocalTextSignal = currentPostLocalTextLsi.signal
    
    div(
      idAttr := "composer-pane",
      // backgroundColor("lightGray"),
      height("calc(100% - 3rem)"),
      overflowY := "clip",
      marginTop.rem(1),
      marginLeft.rem(1),
      marginRight.rem(1),
      div(
        idAttr := "compose-text-and-preview-card",
        display.flex,
        flexDirection.column,
        height.percent(100),
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
              fontWeight.bold,
              "Content Type: "
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
              onChange.mapToValue --> { (value) => currentPostLocalContentTypeLsi.set(value) },
              value <-- currentPostLocalContentTypeSignal
            )
          ),
          div(
            flexGrow(1),
            textAlign.center,
            TinyLink.create("edit")
          ),
          div(
            flexGrow(1),
            textAlign.center,
            TinyLink.create("preview")
          ),
          div(
            paddingLeft.rem(0.5),
            statusCircle(),
          ),
          div(
            paddingLeft.rem(0.5),
            statusCircle(),
          )
        ),
        styleTag(
          """
          |#composer-text-area:focus {
          |  border-color: black;
          |  outline: none;
          |}
          """.stripMargin
        ),
        div(
          flexGrow(1),
          textArea(
            idAttr := "composer-text-area",
            fontFamily("monospace"),
            padding.rem(1),
            borderStyle.solid,
            borderColor.black,
            borderWidth.px(2),
            borderRadius.px(10),
            width.percent(100),
            height.percent(100),
            resize("none"),
          )
        )
      )
    )
