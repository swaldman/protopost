package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import protopost.common.api.{PostDefinition,PostMediaInfo,PostRevisionHistory}

import sttp.model.Uri
import sttp.client4.WebSocketBackend

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import com.mchange.conveniences.string.*

object PublishDetailsPane:
  def create( client : Client, idPrefix : String ) : HtmlElement =
    import Client.PublishDetailsPaneLabelCommonModifiers
    import client.*

    val subsectionLabelModifiers = Seq( fontSize.pt(10), fontWeight.bold )

    val fileUploadComponentsVal : Var[(Option[String],Option[dom.File])] = Var(Tuple2(None,None))
    val fileUploadComponentsSignal = fileUploadComponentsVal.signal
    val fileUploadComponentsDistinctChanges = fileUploadComponentsSignal.changes.distinct

    val publicationAttemptedSignal     = currentPostDefinitionSignal.map( _.fold(false)( _.publicationAttempted ) )
    val publishUpdateButtonLabelSignal = publicationAttemptedSignal.map( pa => if pa then "update post" else "publish post" )

    val publicationStatusSpanSignal =
      val unknownSpan     = span("Unknown.")
      val unpublishedSpan = span("Unpublished.")
      val noPermalinkSpan = span("Publication attempted, awaiting permalink.")
      val mbPermalinkSignal =
        currentPostDefinitionSignal.map: mbpd =>
          mbpd.flatMap: pd =>
            if pd.publicationAttempted then pd.htmlPermalink else None
      val publishedSpan =
        span(
          a(
            href <-- mbPermalinkSignal.map( _.getOrElse("javascript:alert('Oops. Bug. pubishedSpan should never be displayed with no permalink')") ),
            "Published"
          ),
          "."
        )
      currentPostDefinitionSignal.map: mbpd =>
        mbpd.fold( unknownSpan ): pd =>
          if pd.publicationAttempted then
            pd.htmlPermalink match
              case Some( permalink ) => publishedSpan
              case None => noPermalinkSpan
          else
            unpublishedSpan

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
      def rowFromPostMediaInfo( id : (Int,String), initial : PostMediaInfo, updates : Signal[PostMediaInfo] ) : Seq[HtmlElement] =
        val (postId, path) = id
        Seq(
          div(a(href:=path,textDecoration.none,path)),
          div(
            text <-- updates.map( pmi => humanReadableByteLength(pmi.length) )
          ),
          div(
            cursor.pointer,
            alignSelf.center,
            color.red,
            marginLeft.rem(0.25),
            "\u00d7",
            onClick --> { _ =>
              util.request.deleteMediaItemForPost(protopostLocation,postId,path,backend,currentPostMediaVar)
            }
          )
        )
      val pmisSignal : Signal[Seq[PostMediaInfo]] =
        currentPostMediaSignal.map: mbpmis =>
          mbpmis match
            case Some(pmis) => pmis
            case None => Seq.empty
      pmisSignal.split( pmi => (pmi.postId, pmi.path) )( rowFromPostMediaInfo ).map( _.flatten )

    val postMediaTableCard =
      val blankHeaderModifiers = Seq(
        fontWeight.bold,
        marginBottom.rem(0.25),
      )
      val textHeaderModifiers =
        blankHeaderModifiers :+ textDecoration.underline
      div(
        div(
          idAttr := "${idPrexix}-post-media-table-card",
          display.grid,
          styleProp("grid-template-columns") := "1fr max-content max-content",
          styleProp("row-gap") := "1fr max-content max-content",
          fontSize.pt(10),
          div(
            textHeaderModifiers,
            "filename or path"
          ),
          div(
            textHeaderModifiers,
            "size"
          ),
          div(
            blankHeaderModifiers,
            "\u00a0"
          ),
          children <-- postMediaTableRowsSignal
        )
      )

    val actionsCard =
      div (
        display.flex,
        flexDirection.row,
        alignContent.center,
        justifyContent.spaceEvenly,
        button(
          cls := "button-utilitarian",
          role("button"),
          disabled <-- notPublishableSignal,
          "mail to self"
        ),
        button(
          cls := "button-utilitarian",
          role("button"),
          disabled <-- notPublishableSignal,
          "request preview"
        ),
        button(
          cls := "button-utilitarian",
          role("button"),
          disabled <-- notPublishableSignal,
          text <-- publishUpdateButtonLabelSignal
        ),
      )

    val actionsReloginCard =
      div(
        flexGrow(1),
        display.flex,
        flexDirection.column,
        alignContent.center,
        justifyContent.center,
        div(
          color.red,
          fontWeight.bold,
          fontSize.pt(10),
          paddingBottom.rem(1.5),
          "You must re-login before publishing, updating, or deleting."
        ),
        LoginForm.create( client ).amend(
          flexGrow(1)
        )
      )

    val actionsCardSignal = loginLevelSignal.map: ll =>
      if ll == LoginLevel.high then
        actionsCard
      else
        actionsReloginCard

    val SectionMarginTopRem = 1
    val SubsectionMarginTopRem = 0.5

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
        PublishDetailsPaneLabelCommonModifiers,
        "status:"
      ),
      div(
        idAttr := "${idPrefix}-publication-status-display",
        child <-- publicationStatusSpanSignal
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        display.flex,
        flexDirection.column,
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "actions:"
        ),
        div(
          sectionBorderPaddingMargin,
          idAttr := "${idPrefix}-publication-actions",
          flexGrow(1),
          display.flex,
          flexDirection.column,
          alignContent.center,
          justifyContent.center,
          child <-- actionsCardSignal
        ),
      ),
      div(
        marginTop.rem(SectionMarginTopRem),
        display.flex,
        flexDirection.column,
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "attributes:"
        ),
        div(
          sectionBorderPaddingMargin,
          display.flex,
          flexDirection.column,
          div(
            display.flex,
            flexDirection.column,
            label(
              subsectionLabelModifiers,
              forId := "${idPrefix}-post-unique-id-input",
              "post anchor (unique ID):"
            ),
            input(
              marginTop.rem(0.25),
              idAttr := "${idPrefix}-post-unique-id-input",
              `type` := "text",
              disabled <-- postNotAnchorableSignal,
              placeholder := "(optional, can only be set once if published) a unique identifier for this post.",
              value <-- postAnchorSignal
            )
          ),
          div(
            marginTop.rem(SubsectionMarginTopRem),
            display.flex,
            flexDirection.column,
            div(
              label(
                subsectionLabelModifiers,
                forId := "${idPrefix}-post-in-reply-to-input",
                "in reply to:"
              )
            ),
            input(
              marginTop.rem(0.25),
              `type` := "text",
              idAttr := "${idPrefix}-post-in-reply-to-input",
              placeholder := "(optional) URL identifying the post to which this is a reply"
            )
          ),
          div(
            marginTop.rem(SubsectionMarginTopRem),
            display.flex,
            flexDirection.column,
            div(
              label(
                subsectionLabelModifiers,
                forId := "${idPrefix}-post-major-update-description-input",
                "major update description:"
              )
            ),
            input(
              marginTop.rem(0.25),
              `type` := "text",
              idAttr := "${idPrefix}-post-major-update-description-input",
              disabled <-- fullyPublishedSignal.map( !_ ),
              placeholder := "(optional, rare) a description of major update"
            )
          ),
          div(
            marginTop.rem(SubsectionMarginTopRem),
            label(
              subsectionLabelModifiers,
              forId := "${idPrefix}-sprout-checkbox",
              "sprout? "
            ),
            input(
              idAttr := "${idPrefix}-sprout-checkbox",
              `type` := "checkbox",
              checked <-- sproutCheckedSignal
            )
          ),
        ),
      ),
      div(
        display.flex,
        flexDirection.column,
        marginTop.rem(SectionMarginTopRem),
        div(
          PublishDetailsPaneLabelCommonModifiers,
          "post media:"
        ),
        div(
          idAttr := "${idPrefix}-post-media-manager",
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
              value <-- fileUploadComponentsDistinctChanges.collect {
                case ( _, None ) => null
              },
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
              },
            ),
            div(
              marginTop.rem(0.5),
              display.flex,
              flexDirection.row,
              label(
                subsectionLabelModifiers,
                forId := "${idPrefix}-post-media-file-path-input",
                "customize filename or path: "
              ),
              input(
                idAttr := "${idPrefix}-post-media-file-path-input",
                marginLeft.rem(0.5),
                flexGrow(1),
                `type` := "text",
                value <-- fileUploadComponentsDistinctChanges.collect {
                  case (Some(fp), _ ) => fp
                  case (None, _ )     => ""
                },
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
      ),
      RevisionsCards.create( client ).amend(
        marginTop.rem(SectionMarginTopRem),
      ),
    )

