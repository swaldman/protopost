package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.collection.immutable

import Client.TinyLinkFontSize
import protopost.api.{DestinationNickname,PostDefinition,PosterNoAuth}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*

object DestinationsAndPostsCard:
  def create(
    protopostLocation : Uri,
    backend : WebSocketBackend[scala.concurrent.Future],
    currentPostDefinitionVar : Var[Option[PostDefinition]],
    destinationsVar : Var[immutable.SortedSet[DestinationNickname]],
    locationVar : Var[Tab],
    posterNoAuthSignal : Signal[Option[PosterNoAuth]]
  ) : HtmlElement =

    object DestinationPane:
      def create( dn : DestinationNickname, initOpen : Boolean = false ) : HtmlElement =
        val openVar : Var[Boolean] = Var(initOpen)
        val destinationText = dn.nickname.getOrElse( s"${dn.destination.name}@${dn.destination.seismicNode.locationUrl}" )

        object PostsPane:
          def create() : HtmlElement =
            div(
                cls("posts-pane"),
                marginLeft.em(2),
                marginTop.rem(0.25),
                fontWeight.normal,
                display <-- openVar.signal.map( open => if open then "block" else "none" ),
              div(
                cls("posts-pane-end-menu"),
                a(
                  fontSize.pt(TinyLinkFontSize),
                  cursor.default,
                  disabled <-- posterNoAuthSignal.map( _.fold(false)(_ => true) ), 
                  onClick.flatMapTo(posterNoAuthSignal) --> { mbPna =>
                    mbPna match
                      case Some(posterNoAuth) =>
                        val postDefinition = new PostDefinition(-1, dn.destination.seismicNode.id, dn.destination.name, posterNoAuth.id, authors = Seq(posterNoAuth.fullName) )
                        util.sttp.hardUpdateNewPostDefinition( protopostLocation, postDefinition, backend, currentPostDefinitionVar )
                        locationVar.set(Tab.currentPost)
                      case None =>
                        println("Cannot create new post, posterNoAuthSignal seems unset? We are not properly logged in?")
                  },
                  "create new post"
                ),
              )
            )

        div(
          // very annoyingly, there's not an easy way to set grid- and hover-related style elements (beyond display.grid itself) in laminar
          styleTag(
            """
            |.posts-pane-end-menu a {
            |  color: blue;
            |}
            |.posts-pane-end-menu a:hover {
            |  color: green;
            |}
            """.stripMargin
          ),
          cls("destination-pane"),
          div(
            fontSize.pt(12),
            span(
              display.inlineBlock,
              marginRight.rem(0.25),
              fontWeight.normal,
              fontSize.smaller,
              verticalAlign.middle,
              text <-- openVar.signal.map( open => if open then "\u25BC" else "\u25B6" ),
              onClick --> { _ => openVar.update(o => !o) }
            ),
            destinationText
          ),
          PostsPane.create()
        )
    end DestinationPane  

    val destinationPanesSignal : Signal[Seq[HtmlElement]] =
      destinationsVar.signal.map: dnset =>
        if dnset.size == 1 then
          List( DestinationPane.create(dnset.head, initOpen = true ) )
        else
          dnset.toVector.map( dn => DestinationPane.create(dn) ).toSeq
    div(
      idAttr("destinations-and-posts-panel"),
      // backgroundColor("green"),
      height.percent(100),
      fontWeight.bold,
      marginLeft.rem(0.5),
      marginRight.rem(0.5),
      div(
        fontSize.pt(18),
        fontWeight.bold,
        "Destinations"
      ),
      util.laminar.blackHr(),
      children <-- destinationPanesSignal
    )

