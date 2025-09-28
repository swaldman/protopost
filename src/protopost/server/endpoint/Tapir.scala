package protopost.server.endpoint

import zio.*

import sttp.model.StatusCode
import sttp.model.headers.{CookieValueWithMeta,Cookie}
import Cookie.SameSite
import sttp.tapir.files.*
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.common.api.{*,given}
import protopost.common.{EmailAddress,Password,PosterId}
import protopost.server.{AppResources,ExternalConfig}
import protopost.server.LoggingApi.*
import protopost.server.exception.{ApparentBug,BadPostDefinition,MissingConfig,ResourceNotFound,UnknownPost}

import protopost.server.db.PgDatabase

import com.mchange.rehash.*

import java.time.Instant
import java.time.temporal.ChronoUnit

import com.mchange.sc.zsqlutil.*
import protopost.server.exception.{BadCookieSettings,BadCredentials,InsufficientPermissions,NotLoggedIn}
import protopost.server.jwt
import sttp.model.headers.CookieValueWithMeta

object Tapir extends SelfLogging:

  val errorHandler =
    def errorBodyOutNotFoundGeneric() = stringBody.map(_ => None)( _ => "Resource not found." )
    def errorBodyOut[T <: Throwable]( throwableClass : Class[T] ) =
      stringBody.map(fst => ReconstructableThrowable(Some(throwableClass),fst))(_.fullStackTrace)
    def errorBodyOutLostThrowableClass() =
      stringBody.map(fst => ReconstructableThrowable(None,fst))(_.fullStackTrace)
    oneOf[ReconstructableThrowable | None.type](
      oneOfVariantValueMatcher(statusCode(StatusCode.NotFound).and(errorBodyOutNotFoundGeneric())){ case _ : None.type => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.NotFound).and(errorBodyOut(classOf[ResourceNotFound]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[ResourceNotFound]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.Forbidden).and(errorBodyOut(classOf[BadCredentials]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[BadCredentials]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.Unauthorized).and(errorBodyOut(classOf[NotLoggedIn]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[NotLoggedIn]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.Unauthorized).and(errorBodyOut(classOf[InsufficientPermissions]))){ case rt : ReconstructableThrowable if rt.throwableClass == Some(classOf[InsufficientPermissions]) => true },
      oneOfVariantValueMatcher(statusCode(StatusCode.InternalServerError).and(errorBodyOutLostThrowableClass())){ case rt : ReconstructableThrowable if rt.throwableClass == None => true },
  )

  val NakedBase = endpoint.errorOut(errorHandler)
  val Base = NakedBase.in("protopost")
  val PosterAuthenticated = Base.securityIn(
    cookie[Option[String]]("token_security_high")
      .and(cookie[Option[String]]("token_security_low"))
      .mapTo[jwt.PosterAuthInfo]
  )

  val Envelope = Base.in("envelope")

  //val RootJwks = Base.in("jwks.json").out(jsonBody[Jwks])
  val WellKnownJwks = NakedBase.in(".well-known").in("jwks.json").out(jsonBody[jwt.Jwks])

  val Login =
    Base.post
      .in("login")
      .in(jsonBody[EmailPassword])
      .out(setCookie("token_security_high"))
      .out(setCookie("token_security_low"))
      .out(jsonBody[LoginStatus])

  val Logout =
    Base.post
      .in("logout")
      .out(setCookie("token_security_high"))
      .out(setCookie("token_security_low"))
      .out(jsonBody[LoginStatus])

  val LoginStatus =
    Base.get
      .in("login-status")
      .in(cookie[Option[String]]("token_security_high"))
      .in(cookie[Option[String]]("token_security_low"))
      .out(jsonBody[LoginStatus])

  val Client = Base.get.in("client").in("top.html").out(htmlBodyUtf8)
  val RootAsClient = NakedBase.in("").get.out(htmlBodyUtf8)

  val PosterInfo = PosterAuthenticated.get.in("poster-info").out(jsonBody[PosterNoAuth])

  val Destinations = PosterAuthenticated.get.in("destinations").out(jsonBody[Set[Destination]])

  val NewPost = PosterAuthenticated.post.in("new-post").in(jsonBody[PostDefinitionCreate]).out(jsonBody[PostDefinition])

  val DestinationPosts = PosterAuthenticated.post.in("destination-posts").in(jsonBody[DestinationIdentifier]).out(jsonBody[Set[PostDefinition]])

  val UpdatePostDefinition = PosterAuthenticated.post.in("update-post").in(jsonBody[PostDefinitionUpdate]).out(jsonBody[PostDefinition])

  val ScalaJsServerEndpoint = staticResourcesGetServerEndpoint[[x] =>> zio.RIO[Any, x]]("protopost"/"client"/"scalajs")(this.getClass().getClassLoader(), "scalajs")

  val NewDraft = PosterAuthenticated.post.in("new-draft").in(jsonBody[NewPostRevision]).out(jsonBody[Option[PostRevisionIdentifier]])

  val LatestDraft = PosterAuthenticated.get.in("latest-draft").in( path[Int] ).out(jsonBody[Option[RetrievedPostRevision]])

  def serverEndpoints( appResources : AppResources ) : List[ZServerEndpoint[Any,Any]] =
    import ServerLogic.*

    val rootAsClient =
      appResources.externalConfig
        .get( ExternalConfig.Key.`protopost.api.root-as-client` )
        .map( java.lang.Boolean.parseBoolean )
        .flatMap( use => if use then Some( RootAsClient.zServerLogic( client( appResources ) ) : ZServerEndpoint[Any,Any] ) else None )
    List[sttp.tapir.ztapir.ZServerEndpoint[Any, Any]] (
      //RootJwks.zServerLogic( jwks( appResources ) ),
      WellKnownJwks.zServerLogic( jwks( appResources ) ),
      Login.zServerLogic( login( appResources ) ),
      LoginStatus.zServerLogic( loginStatus( appResources ) ),
      Logout.zServerLogic( logout( appResources ) ),
      Client.zServerLogic( client( appResources ) ),
      PosterInfo.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( posterInfo(appResources) ),
      Destinations.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( destinations(appResources) ),
      NewPost.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( newPost(appResources) ),
      DestinationPosts.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( destinationPosts(appResources) ),
      UpdatePostDefinition.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( updatePostDefinition(appResources) ),
      ScalaJsServerEndpoint,
      NewDraft.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( newDraft( appResources ) ),
      LatestDraft.zServerSecurityLogic( authenticatePoster(appResources) ).serverLogic( latestDraft( appResources ) ),
    ) ++ rootAsClient.toList

end Tapir
