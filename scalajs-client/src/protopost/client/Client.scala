package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

// This starting-point laminar application is modified from
// https://github.com/raquo/laminar-full-stack-demo/blob/master/client/src/main/scala/com/raquo/app/basic/HelloWorldView.scala
object Client {

  @main
  def main() : Unit =
    lazy val container = dom.document.getElementById("root")
    render(container, top())
    //println("Hello.")

  def top(): HtmlElement = {
    div(
      renderExample(),
    )
  }

  def renderExample(): HtmlElement = {
    // #Exercise for the reader:
    // What will change if we move nameVar to be
    // a member of object HelloWorldView
    // (outside of the `renderExample` method) and why?
    // HINT: htrof dna kcab gnitagivan yrt

    // BEGIN[hello world]
    val nameVar = Var(initial = "world")
    div(
      label("Your name: "),
      input(
        placeholder := "Enter your name here",
        onInput.mapToValue --> nameVar
      ),
      p(
        "Hello, ",
        text <-- nameVar.signal.map(_.toUpperCase)
      )
    )
    // END[hello world]
  }
}
