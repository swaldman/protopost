package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import protopost.api.DestinationNickname

import scala.collection.immutable

import Client.TinyLinkFontSize

object DestinationsAndPostsCard:
  def create( destinationsVar : Var[immutable.SortedSet[DestinationNickname]] ) : HtmlElement =
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

  object DestinationPane:
    def create( dn : DestinationNickname, initOpen : Boolean = false ) : HtmlElement =
      val openVar : Var[Boolean] = Var(initOpen)
      val destinationText = dn.nickname.getOrElse( s"${dn.destination.name}@${dn.destination.seismicNode.locationUrl}" )

      object PostsPane:
        def create(dn : DestinationNickname) : HtmlElement =
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
        PostsPane.create(dn)
      )


