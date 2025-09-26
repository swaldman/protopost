package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.collection.immutable

import com.mchange.conveniences.string.*

import protopost.common.api.{Destination,PostDefinition,PostDefinitionCreate,PostIdentifier,PosterNoAuth}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
import protopost.common.api.DestinationIdentifier

object DestinationsAndPostsCard:
  def create(
    protopostLocation : Uri,
    backend : WebSocketBackend[scala.concurrent.Future],
    currentPostIdentifierLocalStorageItem : LocalStorageItem[Option[PostIdentifier]],
    destinationsVar : Var[immutable.SortedSet[Destination]],
    destinationsToKnownPostsVar : Var[Map[DestinationIdentifier,Map[Int,PostDefinition]]],
    locationLocalStorageItem : LocalStorageItem[TopPanel.Tab],
    posterNoAuthSignal : Signal[Option[PosterNoAuth]]
  ) : HtmlElement =
    val currentPostIdentifierSignal = currentPostIdentifierLocalStorageItem.signal

    object DestinationPane:
      def create( destination : Destination, initOpen : Boolean = false ) : HtmlElement =
        // println( s"DestinationPane.create( $destination, $initOpen )" )
        val openVar : Var[Boolean] = Var(initOpen)
        val openSignal = openVar.signal
        val destinationText = destination.nickname.getOrElse( s"${destination.name}@${destination.seismicNode.locationUrl}" )

        object PostsPane:
          val postDefinitionsSignal : Signal[Option[immutable.SortedSet[PostDefinition]]] =
            given Ordering[PostDefinition] = ReverseChronologicalPostDefinitions
            destinationsToKnownPostsVar.signal.map: outerMap =>
              outerMap.get(destination.destinationIdentifier).map: destinationMap =>
                val pds = destinationMap.map( (k,v) => v )
                pds.to(immutable.SortedSet)

          val openPostDefinitionsSignal = Signal.combine(openSignal,postDefinitionsSignal)

          val postsOpenObserver = Observer[(Boolean,Option[immutable.SortedSet[PostDefinition]])]: (open : Boolean, mbpdset : Option[immutable.SortedSet[PostDefinition]]) =>
              if open && mbpdset.isEmpty then updatePosts()

          val newPostCreatedObserver = Observer[(Option[PostIdentifier],Option[immutable.SortedSet[PostDefinition]])]( (mbPostIdentifier, mbCurrentPostDefinitions) => 
            mbPostIdentifier match
              case Some( postIdentifier ) =>
                if postIdentifier.destinationIdentifier == destination.destinationIdentifier then
                  mbCurrentPostDefinitions match
                    case Some(currentPostDefinitions) if !currentPostDefinitions.exists( _.postId == postIdentifier.postId ) => updatePosts()
                    case _ => /* ignore */
              case None =>
                /* ignore */
          )

          val newPostClickBus = new EventBus[dom.MouseEvent]
          val newPostClicksWithPoster = newPostClickBus.events.withCurrentValueOf(posterNoAuthSignal)

          val newPostClicksObserver = Observer[(dom.MouseEvent,Option[PosterNoAuth])]: (_,mbPna) =>
            mbPna match
              case Some(posterNoAuth) =>
                val di = destination.destinationIdentifier
                val postDefinition = new PostDefinitionCreate( destination.seismicNode.id, destination.name, posterNoAuth.id, authors = Seq(posterNoAuth.fullName) )
                util.request.hardUpdateNewPostDefinition( protopostLocation, di, postDefinition, backend, destinationsToKnownPostsVar, currentPostIdentifierLocalStorageItem )
                locationLocalStorageItem.set(TopPanel.Tab.currentPost)
              case None =>
                println("Cannot create new post, posterNoAuthSignal seems unset? We are not properly logged in?")

          def updatePosts() =
            util.request.hardUpdateDestinationsToKnownPosts( protopostLocation, destination.destinationIdentifier, backend, destinationsToKnownPostsVar )

          private def postDiv( pd : PostDefinition ) : HtmlElement =
            val title = pd.title.fold(Client.UntitledPostLabel)(t => s""""$t"""")
            val authors = commaListAnd( pd.authors ).fold("")( authors => s"by ${authors}" )
            div(
              fontSize.pt(10),
              ClickLink.create(title).amend(
                onClick --> { _ =>
                  currentPostIdentifierLocalStorageItem.set(Some(PostIdentifier(destination.destinationIdentifier,pd.postId)))
                  locationLocalStorageItem.set(TopPanel.Tab.currentPost)
                }
              ),
              " ",
              span(
                authors,
              ),
              " ",
              span(
                fontSize.pt(8),
                s"[Post ID #${pd.postId}]"
              )
            )
          def create() : HtmlElement =
            // println( s"PostsPane.create( $destination, $initOpen )" )
            div(
              cls("posts-pane"),
              marginLeft.rem(1),
              marginTop.rem(0.25),
              fontWeight.normal,
              display <-- openSignal.map( open => if open then "block" else "none" ),
              children <-- {postDefinitionsSignal.map: mbPdset =>
                mbPdset match
                  case Some( pdset ) => pdset.toList.map(pd => postDiv(pd))
                  case None => List.empty
              },
              onMountCallback { mountContext =>
                // println( s"mount: $mountContext" )
                given Owner = mountContext.owner
                openPostDefinitionsSignal.addObserver( postsOpenObserver )
                currentPostIdentifierSignal.withCurrentValueOf(postDefinitionsSignal).addObserver( newPostCreatedObserver )
                newPostClicksWithPoster.addObserver( newPostClicksObserver )
              },
              // onUnmountCallback { mountContext =>
              //   println( s"unmount: $mountContext" )
              // },
              div(
                marginLeft.rem(0.25),
                verticalAlign.middle,
                "\u00BB ",
                ClickLink.create("create new post").amend(
                  fontSize.pt(10),
                  cls("posts-pane-end-menu"),
                  //disabled <-- posterNoAuthSignal.map( _.fold(false)(_ => true) ),
                  onClick --> newPostClickBus,
                ),
                " \u00AB",
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
            fontSize.pt(11),
            fontWeight.bold,
            span(
              display.inlineBlock,
              marginRight.rem(0.25),
              fontWeight.normal,
              fontSize.pt(8),
              verticalAlign.middle,
              cursor.pointer,
              text <-- openVar.signal.map( open => if open then /*"\u25BE"*/ "\u25BC" else /*"\u25B8"*/ "\u25B6" ),
              onClick --> { _ => openVar.update(o => !o) }
            ),
            destinationText
          ),
          PostsPane.create()
        )
    end DestinationPane

    val destinationPanesSignal : Signal[Seq[HtmlElement]] =
      destinationsVar.signal.map: dset =>
        if dset.size == 1 then
          List( DestinationPane.create(dset.head, initOpen = true ) )
        else
          dset.toVector.map( destination => DestinationPane.create(destination) ).toSeq
    div(
      idAttr("destinations-and-posts-panel"),
      // backgroundColor("green"),
      height.percent(100),
      paddingLeft.rem(Client.CardPaddingLeftRightRem),
      paddingRight.rem(Client.CardPaddingLeftRightRem),
      div(
        fontSize.pt(Client.CardTitleFontSizePt),
        fontWeight.bold,
        "Destinations"
      ),
      util.laminar.blackHr(),
      children <-- destinationPanesSignal
    )

