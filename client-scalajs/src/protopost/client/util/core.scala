package protopost.client.util

import protopost.client.{marked,DOMPurify}

def epochSecondsNow() : Long = System.currentTimeMillis()/1000

def safeHtmlFromUserHtml( userHtml : String ) : String = DOMPurify.sanitize( userHtml )

def safeHtmlFromMarkdown( userMarkdown : String ) : String = marked.parse( DOMPurify.sanitize( userMarkdown ) )

def urlEncode( raw : String ) : String =
  import java.net.URLEncoder
  import java.nio.charset.StandardCharsets
  URLEncoder.encode(raw, StandardCharsets.UTF_8.toString)


