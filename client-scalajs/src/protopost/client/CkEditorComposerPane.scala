package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

import scala.util.{Success,Failure}
import protopost.common.api.PostDefinition

object CkEditorComposerPane:

  val ckEditorVar : Var[Option[CkEditor]] = Var(None)
  val ckEditorSignal = ckEditorVar.signal

  private object LockId:
    val BadContentOrComposer = "bad-content-or-composer"

  def create( client : Client ) : HtmlElement =
    import client.*

    val ckEditorContainer =
      div(
        idAttr := "ckeditor-container",
        flexGrow(1),
        height.px(0),
        maxHeight.percent(100),
        overflowY.scroll,
      )

    div(
      flexGrow(1),
      display.flex,
      flexDirection.column,
      ckEditorContainer,
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        given Owner = mountContext.owner
        if ckEditorSignal.now().isEmpty then
          Globals.bindCkEditor( "ckeditor-container" )
            .toFuture
            .onComplete: attempt =>
              attempt match
                case Success( ckEditor ) =>
                  def updateData() = currentPostLocalPostContentLsi.update( _.copy(text=ckEditor.getData()) )
                  ckEditor.model.document.on("change:data", _ => updateData())
                  println("Set up event callback.")
                  ckEditorVar.set( Some( ckEditor ) )
                case Failure( t ) =>
                  t.printStackTrace()
        val loadContentObserver = Observer[Tuple3[PostContent,Option[CkEditor],Composer]]: (pc, mbcke, composer) =>
          println( "disableNonHtmlObserver" )
          (composer, mbcke) match
            case (Composer.WYSIWYG, Some( cke )) =>
              if pc.contentType == "text/html" then
                if cke.getData() != pc.text then cke.setData( pc.text )
                cke.disableReadOnlyMode( LockId.BadContentOrComposer )
              else
                cke.setData("""<div style="text-align: center; "><p style="color: red;"><b>This post contains non-HTML content, cannot edit with WYSIWYG composer.</b></p><p>Choose a different composer under "profile".</p></div>""")
                cke.enableReadOnlyMode( LockId.BadContentOrComposer )
            case (Composer.`text-and-preview`, Some( cke )) =>
                cke.setData("""<div style="text-align: center; "><b>WYSIWYG composer disabled by in profile by user.</b></div>""")
                cke.enableReadOnlyMode( LockId.BadContentOrComposer )
            case ( _, None) =>
              /* ignore */
        Signal.combine(currentPostLocalPostContentSignal.distinct,ckEditorVar.distinct,composerSignal.distinct).addObserver( loadContentObserver )
      },
    )
