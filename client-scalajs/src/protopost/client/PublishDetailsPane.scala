package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import protopost.common.api.PostDefinition
import protopost.common.api.PostRevisionHistory

import sttp.model.Uri
import sttp.client4.WebSocketBackend

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import com.mchange.conveniences.string.*

object PublishDetailsPane:
  def create( client : Client ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

    val fileUploadComponentsVal : Var[(Option[String],Option[dom.File])] = Var(Tuple2(None,None))
    val fileUploadComponentsSignal = fileUploadComponentsVal.signal

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
            pmis.flatMap: pmi =>
              Seq(
                div(a(href:=pmi.path,textDecoration.none,pmi.path)),
                div(humanReadableByteLength(pmi.length)),
                div(
                  cursor.pointer,
                  alignSelf.center,
                  color.red,
                  marginLeft.rem(0.25),
                  "\u00d7",
                  onClick --> { _ =>
                    util.request.deleteMediaItemForPost(protopostLocation,pmi.postId,pmi.path,backend,currentPostMediaVar)
                  }
                )
              )
          case None =>
            Seq.empty

    val postMediaTableCard = div(
      div(
        idAttr := "post-media-table-card",
        display.grid,
        styleProp("grid-template-columns") := "1fr max-content max-content",
        styleProp("row-gap") := "1fr max-content max-content",
        fontSize.pt(10),
        div(
          fontWeight.bold,
          "filename or path"
        ),
        div(
          fontWeight.bold,
          "size"
        ),
        div(
          "\u00a0"
        ),
        children <-- postMediaTableRowsSignal
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
          marginTop.rem(0.25),
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
          marginTop.rem(0.25),
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
          marginTop.rem(0.25),
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
          sectionBorderPaddingMargin,
          div(
            display <-- currentPostMediaSignal.map( _.fold("none")(pmis => if pmis.isEmpty then "none" else "block") ),
            postMediaTableCard
          ),
          util.laminar.blackHr(),
          div(
            marginTop.rem(0.5),
            fontSize.pt(10),
            fontWeight.bold,
            display.flex,
            flexDirection.column,
            input(
              fontFamily("sans-serif"),
              `type` := "file",
              onChange.mapToFiles --> { files =>
                if files.length > 0 then
                  if files.length > 1 then dom.console.warn(s"Expected one selected file, found ${files.length}, using first: ${files}")
                  fileUploadComponentsVal.update: fuc =>
                    if fuc._1.isEmpty then
                      fuc.copy( _1 = Some( files.head.name ), _2 = Some(files.head) )
                    else
                      fuc.copy( _2 = Some(files.head) )
                else
                  fileUploadComponentsVal.update( _.copy( _2 = None ) )
              }
            ),
            div(
              marginTop.rem(0.5),
              display.flex,
              flexDirection.row,
              label(
                forId := "post-media-file-path-input",
                "customize filename or path: "
              ),
              input(
                marginLeft.rem(0.5),
                flexGrow(1),
                `type` := "text",
                value <-- fileUploadComponentsSignal.changes.distinct.collect { case (Some(fp), _ ) => fp },
                onChange.mapToValue --> { filePath =>
                  val trimmed = filePath.trim
                  if trimmed.nonEmpty then
                    fileUploadComponentsVal.update( _.copy( _1 = Some(trimmed) ) )
                  else
                    fileUploadComponentsVal.update( _.copy( _1 = None ) )
                }
              )
            ),
            button(
              cls("button-utilitarian"),
              marginTop.rem(0.5),
              disabled <-- fileUploadComponentsSignal.map( (mbfp, mbf) => mbfp.isEmpty || mbf.isEmpty ),
              "upload",
              alignSelf.flexStart,
              onClick( _.withCurrentValueOf(fileUploadComponentsSignal,currentPostDefinitionSignal) ) --> { (_,tup,mbpd) =>
                tup match
                  case (Some(filePath),Some(file)) =>
                    mbpd match
                      case Some(pd) =>
                        util.request.writeMediaItemForPost(protopostLocation,pd.postId,filePath,file,backend,currentPostMediaVar)
                        fileUploadComponentsVal.set(Tuple2(None,None))
                      case None     => dom.console.error(s"Tried to upload ${file.name} to $filePath, but no current definition has been set?")
                  case _ =>
                    dom.console.error(s"Tried to upload a media item, but no at least one of filePath and file are not set: ${tup}")
              }
            )
          )
        )
      )
    )

