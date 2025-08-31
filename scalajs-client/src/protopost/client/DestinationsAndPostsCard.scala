package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.collection.immutable

import protopost.api.{DestinationNickname,PostDefinition,PostDefinitionCreate,PosterNoAuth}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
import protopost.api.DestinationIdentifier

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
        // println( s"DestinationPane.create( $dn, $initOpen )" )
        val openVar : Var[Boolean] = Var(initOpen)
        val openSignal = openVar.signal
        val destinationText = dn.nickname.getOrElse( s"${dn.destination.name}@${dn.destination.seismicNode.locationUrl}" )

        object PostsPane:
          val postDefinitionsVar : Var[immutable.SortedSet[PostDefinition]] =
            given Ordering[PostDefinition] = ReverseChronologicalPostDefinitions // also set up in util.hardUpdatePostDefinitionsForDestination
            Var(immutable.SortedSet.empty)

          val postsOpenObserver : Observer[(Boolean,immutable.SortedSet[PostDefinition])] =
            Observer[(Boolean,immutable.SortedSet[PostDefinition])]: (open : Boolean, pdset : immutable.SortedSet[PostDefinition]) =>
              if open && pdset.isEmpty then updatePostsList()

          val newPostCreatedObserver = Observer[(Option[PostDefinition],immutable.SortedSet[PostDefinition])]( (mbPostDefinition, currentPostDefinitions) => // currentPostDefinitinVar
            mbPostDefinition match
              case Some( postDefinition ) =>
                if postDefinition.destination == dn.destination && !currentPostDefinitions(postDefinition) then updatePostsList()
              case None =>
                /* ignore */
          )

          val newPostClickBus = new EventBus[dom.MouseEvent]
          val newPostClicksWithPoster = newPostClickBus.events.withCurrentValueOf(posterNoAuthSignal)

          val newPostClicksObserver = Observer[(dom.MouseEvent,Option[PosterNoAuth])]: (_,mbPna) =>
            mbPna match
              case Some(posterNoAuth) =>
                val postDefinition = new PostDefinitionCreate( dn.destination.seismicNode.id, dn.destination.name, posterNoAuth.id, authors = Seq(posterNoAuth.fullName) )
                util.sttp.hardUpdateNewPostDefinition( protopostLocation, postDefinition, backend, currentPostDefinitionVar )
                locationVar.set(Tab.currentPost)
              case None =>
                println("Cannot create new post, posterNoAuthSignal seems unset? We are not properly logged in?")

          def updatePostsList() =
            util.sttp.hardUpdatePostDefinitionsForDestination( protopostLocation, DestinationIdentifier( dn.destination.seismicNode.id, dn.destination.name), backend, postDefinitionsVar )

          private def postDiv( pd : PostDefinition ) : HtmlElement =
            val title = pd.title.fold("(untitled post)")(t => s""""$t"""")
            val authors = commaListAnd( pd.authors ).fold("")( authors => s"by ${authors}" )
            div(
              fontSize.pt(10),
              ClickLink.create(title).amend(
                onClick --> { _ =>
                  currentPostDefinitionVar.set(Some(pd))
                  locationVar.set(Tab.currentPost)
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
            // println( s"PostsPane.create( $dn, $initOpen )" )
            div(
              cls("posts-pane"),
              marginLeft.rem(1),
              marginTop.rem(0.25),
              fontWeight.normal,
              display <-- openSignal.map( open => if open then "block" else "none" ),
              children <-- postDefinitionsVar.signal.map( pdset => pdset.toList.map(pd => postDiv(pd)) ),
              onMountCallback { mountContext =>
                // println( s"mount: $mountContext" )
                given Owner = mountContext.owner
                openSignal.withCurrentValueOf(postDefinitionsVar).addObserver( postOpensObserver )
                currentPostDefinitionVar.signal.withCurrentValueOf(postDefinitionsVar).addObserver( newPostCreatedObserver )
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
            fontSize.pt(12),
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
      destinationsVar.signal.map: dnset =>
        if dnset.size == 1 then
          List( DestinationPane.create(dnset.head, initOpen = true ) )
        else
          dnset.toVector.map( dn => DestinationPane.create(dn) ).toSeq
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

