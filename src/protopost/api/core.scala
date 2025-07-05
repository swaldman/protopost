package protopost.api

import zio.*

import com.mchange.conveniences.throwable.*

// stealing some utilities from https://github.com/swaldman/hotsauce-devilla

type ZOut[T] = ZIO[Any,Option[String],T]

def mapPlainError[U]( task : Task[U] ) : ZOut[U] = task.mapError( t => Some( t.fullStackTrace ) )

def mapMaybeError[U]( task : Task[Option[U]] ) : ZOut[U] =
  mapPlainError( task ).flatMap:
    case Some( u ) => ZIO.succeed( u )
    case None      => ZIO.fail[Option[String]]( None )


