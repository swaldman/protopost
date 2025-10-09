package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import scala.util.{Success,Failure}
import protopost.common.api.PostDefinition

object CkEditorComposerPane:
  @js.native
  @JSGlobalScope
  object CkGlobals extends js.Object:
    def bindCkEditor( mainContainerId : String, toolbarContainerId : String ) : js.Promise[CkEditor] = js.native
  
  @js.native
  trait CkEditorDocument extends js.Object:
    def on( eventType : String, callback : js.Function1[js.Object,Unit]) : Unit = js.native

  @js.native
  trait CkEditorModel extends js.Object:
    val document : CkEditorDocument = js.native

  @js.native
  @JSGlobal
  class CkEditor extends js.Any:
    def getData()                              : String = js.native
    def setData( htmlText : String )           : Unit   = js.native
    def enableReadOnlyMode( lockId : String )  : Unit   = js.native
    def disableReadOnlyMode( lockId : String ) : Unit   = js.native
    val model : CkEditorModel = js.native

    def on( eventType : String, callback : js.Function1[Object,Unit]) : Unit   = js.native

  enum Tab:
    case edit, publish

  val ckEditorVar : Var[Option[CkEditor]] = Var(None)
  val ckEditorSignal = ckEditorVar.signal

  private object LockId:
    val BadContentOrComposer = "bad-content-or-composer"

  def create( client : Client ) : HtmlElement =
    import client.*

    def tabsDiv() = util.laminar.tabsDiv[Tab]("ckeditor-composer-pane-tabs",Tab.values,ckEditorComposerPaneCurrentTabVar,ckEditorComposerPaneCurrentTabSignal)

    def tabsButtonsStatusDiv() =
      div(
        marginBottom.rem(0.5),
        idAttr := "ckcompose-text-and-preview-tabs-button-status",
        display.flex,
        flexDirection.row,
        alignItems.center,
        justifyContent.center,
        tabsDiv().amend(
          flexGrow(1)
        ),
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
      )

    val ckEditorContainer =
      div(
        idAttr := "ckeditor-container",
        flexGrow(1),
        height.px(0),
        maxHeight.percent(100),
        overflowY.scroll,
      )

    val ckEditorCard =
      div(
        padding.rem(1),
        borderStyle.solid,
        borderColor.black,
        borderWidth.px(2),
        borderRadius.px(10),
        flexGrow(1),
        display.flex,
        flexDirection.column,
        styleTag(
          s"""|:root {
              |  --ck-content-font-family: ${serifFontFamilies};
              |  --ck-content-font-size:   12pt;
              |  --ck-content-line-height: 15pt;
              |}""".stripMargin
        ),
        div(
          idAttr := "ckeditor-toolbar-container",
          display.flex,
          flexDirection.column,
        ),
        ckEditorContainer
      )

    val publishCard = PublishDetailsPane.create( client ).amend( flexGrow(1) )

    val reloginCard = LoginForm.create( client ).amend( flexGrow(1) )

    div(
      marginTop.rem(1),
      marginLeft.rem(1),
      marginRight.rem(1),
      flexGrow(1),
      display.flex,
      flexDirection.column,
      tabsButtonsStatusDiv(),
      //ckEditorCard,
      child <-- Signal.combine(ckEditorComposerPaneCurrentTabSignal,loginLevelSignal).map { (tab,ll) =>
                   tab match
                     case Tab.edit                             => ckEditorCard
                     case Tab.publish if ll == LoginLevel.high => publishCard
                     case Tab.publish                          => reloginCard
      },
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        given Owner = mountContext.owner
        if ckEditorSignal.now().isEmpty then
          CkGlobals.bindCkEditor( "ckeditor-container", "ckeditor-toolbar-container" )
            .toFuture
            .onComplete: attempt =>
              attempt match
                case Success( ckEditor ) =>
                  println(s"ckEditor: $ckEditor")
                  def updateLocalContent() =
                    currentPostLocalPostContentLsi.update: pc =>
                      // don't update if we're just displaying a placeholder
                      if pc.contentType == "text/html" && composerLsi.now() != Composer.`text-and-preview` then 
                        val out = pc.copy(text=ckEditor.getData())
                        localContentDirtyVar.set(true)
                        out
                      else
                        pc
                  ckEditor.model.document.on("change:data", _ => updateLocalContent())
                  println("Set up event callback.")
                  ckEditorVar.set( Some( ckEditor ) )
                case Failure( t ) =>
                  t.printStackTrace()
        val loadContentObserver = Observer[Tuple3[PostContent,Option[CkEditor],Composer]]: (pc, mbcke, composer) =>
          // println( "loadContentObserver" )
          (composer, mbcke) match
            case (Composer.WYSIWYG, Some( cke )) =>
              if pc.contentType == "text/html" then
                if cke.getData() != pc.text then cke.setData( pc.text )
                cke.disableReadOnlyMode( LockId.BadContentOrComposer )
              else
                cke.setData(
                  """<div style="text-align: center; "><p style="color: red;"><b>This post contains non-HTML content, cannot edit with WYSIWYG composer.</b></p><p>Choose a different composer under "profile".</p></div>"""
                )
                cke.enableReadOnlyMode( LockId.BadContentOrComposer )
            case (Composer.`text-and-preview`, Some( cke )) =>
                cke.setData(
                  """<div style="text-align: center; "><b>WYSIWYG composer disabled by in profile by user.</b></div>"""
                )
                cke.enableReadOnlyMode( LockId.BadContentOrComposer )
            case ( _, None) =>
              /* ignore */
        Signal.combine(currentPostLocalPostContentSignal.distinct,ckEditorVar.distinct,composerSignal.distinct).addObserver( loadContentObserver )
      },
    )
