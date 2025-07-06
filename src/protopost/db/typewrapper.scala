package protopost.db

import scala.annotation.targetName

object PosterId:
  def apply( i : Int ) : PosterId = i
opaque type PosterId = Int

extension ( pid : PosterId )
  @targetName("posterIdToLong") inline def toInt : Int = pid


