package protopost.client

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

import sttp.client4.*
import sttp.client4.fetch.*
import sttp.client4.jsoniter.*
import sttp.model.*

import scala.collection.immutable
import scala.util.{Success,Failure}

import scala.scalajs.concurrent.JSExecutionContext.Implicits.*
//import scala.scalajs.js.timers.*

import protopost.common.api.{*,given}
import protopost.client.util.epochSecondsNow

import scala.util.control.NonFatal
import com.raquo.laminar.nodes.ReactiveHtmlElement

object TopPanel:

  private final val TopPanelMargin = 2 //2px

  object Tab:
    import com.github.plokhotnyuk.jsoniter_scala.core.* 
    import com.github.plokhotnyuk.jsoniter_scala.macros.*
    given JsonValueCodec[Tab]= JsonCodecMaker.make
  enum Tab( val label : String ):
    //case newPost  extends Tab("new post")
    case destinationsAndPosts extends Tab("destinations and posts")
    case currentPost          extends Tab("current post")
    case profile              extends Tab("profile")

  def create(client : Client) : HtmlElement =
    import client.*

    val loggedInLocationSignal = topPanelLocationSignal.combineWithFn(loginLevelSignal): ( loc, level ) =>
      level match
        case LoginLevel.high | LoginLevel.low => Some(loc)
        case _ => None

    val disabledTopPanelTabsSignal = currentPostIdentifierSignal.map: mbPi =>
      mbPi match
        case Some(pi) => Set.empty[TopPanel.Tab]
        case None => Set(TopPanel.Tab.currentPost)

    val loginForm = LoginForm.create( client )

    val destinationsAndPostsCard = DestinationsAndPostsCard.create( client )
    val currentPostCard = CurrentPostCard.create( client )
    val profileCard = ProfileCard.create( client )

    val logoutSubmitter = Observer[dom.MouseEvent]: tup =>
      val transformation : LoginStatus => Option[(LoginStatus,Long,Boolean)] = loginStatus => Some(Tuple3(loginStatus, epochSecondsNow(),false))
      val request = basicRequest.post( protopostLocation.addPath("protopost","logout") ).response(asJson[LoginStatus])
      util.request.setVarFromTransformedApiResult( request, backend, loginStatusVar, transformation )

    def createTab( tab : Tab ) : HtmlElement =
      div(
        cls := "tab-pane",
        idAttr := "tab-pane-${tab}",
        cls <-- topPanelLocationSignal.map( t => if t == tab then "current" else "" ),
        cls <-- disabledTopPanelTabsSignal.map( tabs => if tabs(tab) then "disabled" else ""),
        textAlign.center,
        TinyLink.create(tab.label).amend(
          onClick( _.withCurrentValueOf(disabledTopPanelTabsSignal).filter((_,disabled) => !disabled(tab)).map(_ => tab)) --> ( t => topPanelLocationLsi.set(t) ),
        ),
      )

    div(
      idAttr("top-panel-frame"),
      borderColor <-- loginLevelSignal.map( _.cssColor ),
      borderStyle.solid,
      borderWidth.px(3),
      margin.px(4),
      loginForm.amend(
        display <-- loggedInLocationSignal.map( _.fold("flex")(_ => "none") )
      ),
      div(
        idAttr("app-panel"),
        width.percent(100),
        height.percent(100), // This is fine since parent now has explicit height
        display <-- loggedInLocationSignal.map {
            case Some(_) => "flex"
            case None => "none"
          },
        div(
          idAttr("app-top-panel"),
          display.flex,
          flexDirection.rowReverse,
          //backgroundColor.black,
          paddingTop.px(4),
          paddingRight.px(4),
          paddingBottom.px(4),
          TinyLink.create("logout").amend(
            idAttr("logout-link"),
            onClick --> logoutSubmitter, //{ event => dom.window.alert("logout") },
          )
        ),
        div(
          // card panel
          idAttr("app-card-panel"),
          cls("scrollbars-hidden"),
          //width.percent(100),
          //height.percent(100),
          flexGrow(1),
          overflowX := "scroll",
          overflowY := "scroll",
          //styleProp("scrollbar-color") := "white",
          //marginTop.auto,
          //marginBottom.auto,
          display.flex,
          destinationsAndPostsCard.amend(
            display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.destinationsAndPosts) then "block" else "none" ),
            flexGrow(1),
          ),
          currentPostCard.amend(
            display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.currentPost) then "flex" else "none" ),
            flexGrow(1),
          ),
          profileCard.amend(
            display <-- loggedInLocationSignal.map( opt => if opt == Some(Tab.profile) then "block" else "none" ),
            flexGrow(1),
          ),
        ),
        div(
          // tab panel
          idAttr("app-tab-panel"),
          display.flex,
          flexDirection.row,
          justifyContent.spaceAround,
          Tab.values.map( t => createTab(t) ),
          marginTop.rem(0.25),
          marginBottom.rem(0.25),

          // why don't these seem to have any effect?
          borderWidth.px(5),
          borderTopWidth.px(5),
          borderBottomWidth.px(5),
          borderLeftWidth.px(5),
          borderRightWidth.px(5),
          borderColor.black,
        )
      )
    )

