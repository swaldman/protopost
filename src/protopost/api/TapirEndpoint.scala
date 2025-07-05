package protopost.api

import zio.*

import sttp.model.StatusCode
import sttp.tapir.ztapir.*
import sttp.tapir.json.jsoniter.*

import protopost.{ExternalConfig,Server}

object TapirEndpoint:

  // stealing from https://github.com/swaldman/hotsauce-devilla
  val either404or500 = oneOf[Option[String]](
    oneOfVariantValueMatcher(statusCode(StatusCode.NotFound).and(stringBody.map(s => None)(_ => "Not Found."))){ case None => true },
    oneOfVariantValueMatcher(statusCode(StatusCode.InternalServerError).and(stringBody.map(s => Some(s))(_.get))){ case Some(_) => true }
  )

  val Base = endpoint.errorOut(either404or500)

  val RootJwks = Base.in("jwks.json").out(jsonBody[Jwks])
  val WellKnownJwks = Base.in(".well-known").in("jwks.json").out(jsonBody[Jwks])

  def jwks( externalConfig : ExternalConfig )(u : Unit) : ZOut[Jwks] =
    val task =
      ZIO.attempt:
        val identity = Server.Identity( externalConfig )
        val jwk = Jwk( identity.publicKey, identity.location )
        Jwks( List( jwk ) )
    mapPlainError( task )

  def serverEndpoints( externalConfig : ExternalConfig ) : List[ZServerEndpoint[Any,Any]] =
    List (
      RootJwks.zServerLogic( jwks( externalConfig ) ),
      WellKnownJwks.zServerLogic( jwks( externalConfig ) ),
    )

end TapirEndpoint
