package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import protopost.common.api.PostDefinition
import protopost.common.api.PostRevisionHistory

import sttp.model.Uri
import sttp.client4.WebSocketBackend

object PublishDetailsPane:
  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

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

    val notPublishableSignal =
      Signal.combine(currentPostDefinitionSignal,localContentDirtyVar).map: ( mbcpd, dirtyLocal ) =>
        mbcpd.isEmpty || dirtyLocal

    val sproutCheckedSignal =
      currentPostDefinitionSignal.map: mbpd =>
        mbpd match
          case Some(pd) => pd.sprout.getOrElse(false)
          case None     => false

    val postMediaTableRowsSignal : Signal[Seq[HtmlElement]] =
      currentPostMediaSignal.map: mbpmis =>
        mbpmis match
          case Some(pmis) =>
            pmis.map: pmi =>
              tr(
                td(pmi.path),
                td(pmi.length),
              )
          case None =>
            Seq.empty

    val postMediaTableCard = div(
      table(
        thead(
          tr(
            th(
              "path"
            ),
            th(
              "size"
            )
          )
        ),
        tbody(
          children <-- postMediaTableRowsSignal
        )
      )
    )

    val SectionMarginTopRem = 1

    div(
      display.flex,
      flexDirection.column,
      marginTop.rem(0.25),
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
          PublishDetailsPaneLabelCommonModifiers,
          flexGrow(1),
          "status:"
        ),
        button(
          cls := "button-utilitarian",
          role("button"),
          float.right,
          disabled <-- notPublishableSignal,
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
          PublishDetailsPaneLabelCommonModifiers,
          "post unique ID:"
        ),
        input(
          idAttr := "post-unique-id-input",
          `type` := "text",
          disabled <-- postNotAnchorableSignal,
          placeholder := "(optional, can only be set once if published) a unique identifier for this post.",
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
            PublishDetailsPaneLabelCommonModifiers,
            "in reply to:"
          )
        ),
        input(
          `type` := "text",
          idAttr := "post-in-reply-to-input",
          placeholder := "(optional) URL identifying the post to which this is a reply"
        )
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        display.flex,
        flexDirection.column,
        div(
          label(
            forId := "post-major-update-description-input",
            PublishDetailsPaneLabelCommonModifiers,
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
          PublishDetailsPaneLabelCommonModifiers,
          "sprout? "
        ),
        input(
          idAttr := "sprout-checkbox",
          `type` := "checkbox",
          checked <-- sproutCheckedSignal
        )
      ),
      RevisionsCards.create( client ).amend(
        marginTop.rem(SectionMarginTopRem),
      ),
      div(
        display.flex,
        flexDirection.column,
        marginTop.rem(SectionMarginTopRem),
        label(
          forId := "post-media-manager",
          PublishDetailsPaneLabelCommonModifiers,
          "post media:"
        ),
        div(
          idAttr := "post-media-manager",
          div(
            display <-- currentPostMediaSignal.map( _.fold("none")(pmis => if pmis.isEmpty then "none" else "block") ),
            postMediaTableCard
          ),
          div(
            input(
              `type` := "file"
            )
          )
        )
      )
    )

