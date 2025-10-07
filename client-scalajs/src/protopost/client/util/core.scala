package protopost.client.util

import protopost.client.{marked,DOMPurify}

def epochSecondsNow() : Long = System.currentTimeMillis()/1000

def safeHtmlFromUserHtml( userHtml : String ) : String = DOMPurify.sanitize( userHtml )

def safeHtmlFromMarkdown( userMarkdown : String ) : String = marked.parse( DOMPurify.sanitize( userMarkdown ) )




