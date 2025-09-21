package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object ComposerPane:
  val contentTypeSelect = "composer-content-type-select"

  def create() : HtmlElement =
    //val postContentType = LocalStorageItem(LocalStorageItem.Key.postContentType, "text/plain")
    //val postText        = LocalStorageItem(LocalStorageItem.Key.postText, "")

    //val postContentTypeSignal = postContentType.signal
    //val postTextSignal        = postText.signal

    div(
      idAttr := "composer-pane",
      // backgroundColor("lightGray"),
      height("calc(100% - 3rem)"),
      overflowY := "clip",
      marginTop.rem(1),
      marginLeft.rem(1),
      marginRight.rem(1),
      display.flex,
      flexDirection.column,

      div(
        marginBottom.rem(0.5),
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
          // onChange.mapToValue --> (value) => postContentType.set(value)
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
        )
      )
    )
