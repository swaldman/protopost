package protopost.api

import sttp.model.StatusCode
import sttp.tapir.ztapir.*

object TapirEndpoint:

  // stealing from https://github.com/swaldman/hotsauce-devilla
  val either404or500 = oneOf[Option[String]](
    oneOfVariantValueMatcher(statusCode(StatusCode.NotFound).and(stringBody.map(s => None)(_ => "Not Found."))){ case None => true },
    oneOfVariantValueMatcher(statusCode(StatusCode.InternalServerError).and(stringBody.map(s => Some(s))(_.get))){ case Some(_) => true }
  )

  val Base = endpoint.errorOut(either404or500)

  val Jwks = Base.in("jwks.json")
  val WellKnownJwks = Base.in(".well-known").in("jwks.json")

end TapirEndpoint
