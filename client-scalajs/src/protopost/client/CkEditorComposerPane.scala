package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object CkEditorComposerPane:

  def create( client : Client ) : HtmlElement =
    import client.*

    div(
      div(
        idAttr := "ckeditor-container"
      ),
      onMountCallback { mountContext =>
        // println( s"mount: $mountContext" )
        // given Owner = mountContext.owner
        Globals.bindCkEditor( "ckeditor-container" ).toFuture.onComplete( attempt => println( s"bindCkEditor: $attempt" ) )
      },
    )
