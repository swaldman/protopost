package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import protopost.common.api.PostDefinition

object PublishDetailsPane:
  def create(
    currentPostDefinitionSignal : Signal[Option[PostDefinition]]
  ) : HtmlElement =

    val publicationAttemptedSignal     = currentPostDefinitionSignal.map( _.fold(false)( _.publicationAttempted ) )
    val publishUpdateButtonLabelSignal = publicationAttemptedSignal.map( pa => if pa then "update post" else "publish post" )

    val publicationStatusSpanSignal = currentPostDefinitionSignal.map: mbpd =>
      mbpd.fold(span("Unknown.")): pd =>
        if pd.publicationAttempted then
          pd.htmlPermalink match
            case Some( permalink ) =>
              span(
                a(
                  href := permalink,
                  "Published"
                ),
                "."
              )
            case None => span("Publication attempted, awaiting permalink.")
        else
          span("Unpublished.")

    val fullyPublishedSignal = currentPostDefinitionSignal.map( _.fold(false)( pd => pd.publicationAttempted && pd.htmlPermalink.nonEmpty ) )

    val postAnchorSignal = currentPostDefinitionSignal.map( _.flatMap( _.postAnchor ).getOrElse("") )

    val postNotAnchorableSignal =
      currentPostDefinitionSignal.map: mbpd =>
        mbpd match
          case Some( pd ) => pd.publicationAttempted && pd.postAnchor.nonEmpty
          case None => true

    val postNotDefinedSignal = currentPostDefinitionSignal.map( _.isEmpty )

    val sproutCheckedSignal =
      currentPostDefinitionSignal.map: mbpd =>
        mbpd match
          case Some(pd) => pd.sprout.getOrElse(false)
          case None     => false

    val labelCommonModifiers = Seq( fontSize.pt(11), fontWeight.bold )

    val SectionMarginTopRem = 1

    div(
      display.flex,
      flexDirection.column,
      marginTop.rem(0.5),
      borderTopColor.black,
      borderTopStyle.solid,
      borderTopWidth.px(2),
      marginLeft.rem(1),
      paddingTop.rem(0.5),
      div(
        display.flex,
        flexDirection.row,
        label(
          forId := "publication-status-display",
          labelCommonModifiers,
          flexGrow(1),
          "status:"
        ),
        button(
          cls := "button-utilitarian",
          role("button"),
          float.right,
          disabled <-- postNotDefinedSignal,
          text <-- publishUpdateButtonLabelSignal
        ),
      ),
      div(
        idAttr := "publication-status-display",
        child <-- publicationStatusSpanSignal
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        display.flex,
        flexDirection.column,
        label(
          forId := "post-unique-id-input",
          labelCommonModifiers,
          "post unique ID:"
        ),
        input(
          idAttr := "post-unique-id-input",
          `type` := "text",
          disabled <-- postNotAnchorableSignal,
          placeholder := "a unique identifier for this post.",
          value <-- postAnchorSignal
        )
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        display.flex,
        flexDirection.column,
        div(
          label(
            forId := "post-in-reply-to-input",
            labelCommonModifiers,
            "In reply to:"
          )
        ),
        input(
          `type` := "text",
          idAttr := "post-in-reply-to-input",
          placeholder := "URL identifying the post to which this is a reply"
        )
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        display.flex,
        flexDirection.column,
        div(
          label(
            forId := "post-major-update-description-input",
            labelCommonModifiers,
            "major update description:"
          )
        ),
        input(
          `type` := "text",
          idAttr := "post-major-update-description-input",
          disabled <-- fullyPublishedSignal.map( !_ ),
          placeholder := "(optional, rare) a description of major update"
        )
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        label(
          forId := "sprout-checkbox",
          labelCommonModifiers,
          "sprout? "
        ),
        input(
          idAttr := "sprout-checkbox",
          `type` := "checkbox",
          checked <-- sproutCheckedSignal
        )
      ),
    )

