package protopost.server.rss

import org.jsoup.Jsoup
import org.jsoup.nodes.{Document as JsoupDocument, Element as JsoupElement}

import sttp.client4.*
import sttp.model.*

import scala.xml.*

import scala.jdk.CollectionConverters.*

import zio.*
import sttp.client4.httpclient.zio.*

import protopost.server.exception.*
import protopost.server.LoggingApi.*
import protopost.server.exception.MismatchedContentType
import protopost.server.rss.ResolvedFeedSource.Html

given LogAdapter = logAdapterFor( "protopost.server.rss" )

val DefaultFeedUpdatePeriodMins = 60 // eventually get rid of this, place it in config

enum FeedType:
  case rss, atom;

object ResolvedFeedSource:
  case class Rss ( href : String, outerTitle : Option[String], elem : Elem ) extends ResolvedFeedSource( "RSS", Some(FeedType.rss), Set("application/rss+xml","application/xml","text/xml") ):
    lazy val innerTitle : Option[String] =
      val nodeSeq = (elem \ "rss" \ "channel" \ "title")
      nodeSeq.size match
        case 0 => None
        case 1 => Some(nodeSeq.head.text)
        case _ =>
          WARNING.log(s"""Found multiple title elements in RSS channel, using first: ${nodeSeq.mkString(", ")}""")
          Some(nodeSeq.head.text)
  case class Atom( href : String, outerTitle : Option[String], elem : Elem ) extends ResolvedFeedSource( "Atom", Some(FeedType.atom), Set("application/atom+xml","application/xml","text/xml") ):
    lazy val innerTitle : Option[String] =
      val nodeSeq = (elem \ "feed" \ "title")
      nodeSeq.size match
        case 0 => None
        case 1 => Some(nodeSeq.head.text)
        case _ =>
          WARNING.log(s"""Found multiple title elements in RSS channel, using first: ${nodeSeq.mkString(", ")}""")
          Some(nodeSeq.head.text)
  case class Html( href : String, outerTitle : Option[String], jsoupDoc : JsoupDocument ) extends ResolvedFeedSource( "HTML", None, Set("text/html") ):
    lazy val innerTitle : Option[String] =
      val elements = jsoupDoc.select("title")
      elements.size match
        case 0 => None
        case 1 => Some(elements.getFirst().text())
        case _ =>
          WARNING.log(s"""Found multiple title elements in RSS channel, using first: ${elements.asScala.mkString(", ")}""")
          Some(elements.getFirst().text())
sealed trait ResolvedFeedSource( typeName : String, val feedType : Option[FeedType], val consistentContentTypes : Set[String] ):
  def outerTitle : Option[String]
  def innerTitle : Option[String]
  def href       : String
  def title      : String = innerTitle.orElse(outerTitle).getOrElse(s"${typeName} feed at ${href}")

case class Feed( title : String, href : String, feedType : FeedType )

private def feedSourceRequest( feedSource : String ) = basicRequest.get( Uri(feedSource) ).response(asStringAlways)

private def resolveFeedSourceFromContent( href : String, outerTitle : Option[String], feedSourceContent : String ) : Task[ResolvedFeedSource] =
  val resolveXml =
    ZIO.attempt:
      val elem = XML.loadString( feedSourceContent )
      elem.label match
        case "rss"  => ResolvedFeedSource.Rss( href, outerTitle, elem )
        case "atom" => ResolvedFeedSource.Atom( href, outerTitle, elem )
        case other  => throw new UnexpectedXmlFeedSource("We do not support XML feeds of type '$other'.")

  val resolveHtml =
    ZIO.attempt:
      val jsoupDoc = Jsoup.parse( feedSourceContent )
      if jsoupDoc.text == feedSourceContent then // that is, if there were no HTML tags
        throw new BadHtmlFeedSource( s"The document beginning '${feedSourceContent.take(30)}...' does not appear to be HTML" )
      else
        ResolvedFeedSource.Html( href, outerTitle, jsoupDoc )

  resolveXml.catchAll: t =>
    TRACE.log("Failed to resolve feed source as XML directly.", t)
    resolveHtml

private def loadFeedSource( sttpClient : SttpClient )( feedSource : String ) : Task[( Option[String], String )] =
  for
    response <- feedSourceRequest(feedSource).send(sttpClient)
  yield
    ( response.contentType, response.body )

private def resolveFeedSource( sttpClient : SttpClient )( feedSource : String, outerTitle : Option[String] ) : Task[ResolvedFeedSource] =
  for
    ( mbContentType, content ) <- loadFeedSource( sttpClient )( feedSource )
    fromContents <- resolveFeedSourceFromContent( feedSource, outerTitle, content )
  yield
    mbContentType.map( _.takeWhile( _ != ';' ).trim ).foreach: ct =>
      if !fromContents.consistentContentTypes(ct) then
        throw new MismatchedContentType( s"Found ${fromContents}, which is not consistent with HTTP Content-Type '$ct'." )
    fromContents

private def pullFeedSourcesFromHtml( `type` : String )( sttpClient : SttpClient )( jsoupDoc : JsoupDocument ) : Set[(href: String, outerTitle: Option[String])] =
  val elements = jsoupDoc.select(s"""link[rel="alternate"][type="${`type`}"]""")

  def extractOuterTitleHref( elem : JsoupElement ) : Option[(href: String, outerTitle: Option[String])] =
    val hrefs = elem.attributes().asList().asScala.filter( _.getKey() == "href" ).map( _.getValue() )
    val ots   = elem.attributes().asList().asScala.filter( _.getKey() == "title" ).map( _.getValue() )
    if hrefs.isEmpty then
      None
    else
      if hrefs.size != 1 then WARNING.log( s"HTML link element contains multiple hrefs, using first: $elem" )
      val href = hrefs.head
      val ot =
        ots.size match
          case 0 => None
          case 1 => Some(ots.head)
          case _ =>
            WARNING.log( s"HTML link element contains multiple titles, using first: $elem" )
            Some(ots.head)
      Some( (href = href, outerTitle = ot) )

  elements.asList().asScala.flatMap( extractOuterTitleHref ).toSet


private def resolveFeedSourcesFromHtml( `type` : String )( sttpClient : SttpClient )( jsoupDoc : JsoupDocument ) : Task[Set[ResolvedFeedSource]] =
  val feedSourceTups = pullFeedSourcesFromHtml( `type` )( sttpClient )( jsoupDoc )
  ZIO.collectAllPar( feedSourceTups.map( tup => resolveFeedSource(sttpClient)(tup(0),tup(1)) ) )

private def resolveFeedSourcesFromHtml( sttpClient : SttpClient )( jsoupDoc : JsoupDocument ) : Task[Set[ResolvedFeedSource]] =
  for
    rss  <- resolveFeedSourcesFromHtml( "application/rss+xml" )( sttpClient )( jsoupDoc )
    atom <- resolveFeedSourcesFromHtml( "application/atom+xml" )( sttpClient )( jsoupDoc )
  yield
    (rss ++ atom)

private def resolveFeedSources( sttpClient : SttpClient )( initial : ResolvedFeedSource ) : Task[Set[ResolvedFeedSource]] =
  initial match
    case ResolvedFeedSource.Html(href, outerTitle, jsoupDoc) =>
      resolveFeedSourcesFromHtml( sttpClient )( jsoupDoc )
    case _ =>
      ZIO.succeed( Set(initial) )

private def feed( rfs : ResolvedFeedSource ) : Option[Feed] =
  val out =
    rfs.feedType map: ft =>
      Feed( title = rfs.title, href = rfs.href, ft )
  if out.isEmpty then
    WARNING.log( s"Probable Bug: Attempted to convert nonfeed feed source to feed, should have resolved these away: ${rfs}" )
  out

def findFeedsFromFeedSource( sttpClient : SttpClient )( feedSource : String, outerTitle : Option[String] = None ) : Task[Set[Feed]] =
  for
    initial <- resolveFeedSource( sttpClient )( feedSource, outerTitle )
    rfsSet  <- resolveFeedSources( sttpClient )( initial )
  yield
    rfsSet.flatMap(feed)
