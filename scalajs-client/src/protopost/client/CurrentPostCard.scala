package protopost.client

import protopost.api.{PostDefinition, given}

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

object CurrentPostCard:
  def create( currentPostDefinitionVar : Var[Option[PostDefinition]] ) : HtmlElement =
    div(
      text <-- currentPostDefinitionVar.signal.map( _.toString() )
    )
