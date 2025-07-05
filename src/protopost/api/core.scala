package protopost.api

import zio.*

import protopost.Server
import protopost.crypto.BouncyCastleSecp256r1

import com.mchange.conveniences.throwable.*

import java.security.interfaces.ECPublicKey

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import sttp.tapir.Schema

// stealing some utilities from https://github.com/swaldman/hotsauce-devilla

type ZOut[T] = ZIO[Any,Option[String],T]

def mapPlainError[U]( task : Task[U] ) : ZOut[U] = task.mapError( t => Some( t.fullStackTrace ) )

def mapMaybeError[U]( task : Task[Option[U]] ) : ZOut[U] =
  mapPlainError( task ).flatMap:
    case Some( u ) => ZIO.succeed( u )
    case None      => ZIO.fail[Option[String]]( None )

object Jwk:
  val DefaultKty = "EC"
  val DefaultCrv = "P-256"
  def apply( publicKey : ECPublicKey, location : Server.Location ) : Jwk =
    Jwk(
      x = BouncyCastleSecp256r1.fieldValueToHex( publicKey.getW().getAffineX() ),
      y = BouncyCastleSecp256r1.fieldValueToHex( publicKey.getW().getAffineY() ),
      kid = location.toUrl,
      kty = DefaultKty,
      crv = DefaultCrv
    )
case class Jwk( x : String, y : String, kid : String, kty : String, crv : String )
case class Jwks( keys : List[Jwk] )

// json codecs -- jsoniter-scala
given JsonValueCodec[Jwk]  = JsonCodecMaker.make
given JsonValueCodec[Jwks] = JsonCodecMaker.make

// json codecs -- tapir
given Schema[Jwk]  = Schema.derived
given Schema[Jwks] = Schema.derived

