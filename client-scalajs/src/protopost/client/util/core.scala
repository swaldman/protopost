package protopost.client.util

import protopost.client.{marked,DOMPurify}
import org.scalajs.dom

def epochSecondsNow() : Long = System.currentTimeMillis()/1000

def safeHtmlFromUserHtml( userHtml : String ) : String = DOMPurify.sanitize( userHtml )

def safeHtmlFromMarkdown( userMarkdown : String ) : String = marked.parse( DOMPurify.sanitize( userMarkdown ) )

def rebasePage( baseHttp : Option[String] ) : Unit =
  val baseTags = dom.document.querySelectorAll("base").map( _.asInstanceOf[dom.HTMLBaseElement] )
  baseTags.size match
    case 0 =>
      val headTag = dom.document.querySelector("head")
      val baseTag = dom.document.createElement("base").asInstanceOf[dom.HTMLBaseElement]
      baseTag.target = "_blank"
      baseTag.href = baseHttp.getOrElse(null)
      headTag.appendChild(baseTag)
    case 1 =>
      baseTags.head.href = baseHttp.getOrElse(null)
      baseTags.head.target = "_blank"
    case n =>
      dom.console.warn(s"Found $n <base> tags, there should only be one, deleting all and resetting.")
      baseTags.foreach(base => base.remove())
      rebasePage( baseHttp )

