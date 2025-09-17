package protopost.server.endpoint

import protopost.{BadSubject,EmailAddress,PosterId}
import scala.util.control.NonFatal

object Subject:
  def parse( stringified : String ) : Subject =
    val pi = stringified.indexOf("|")
    if pi >= 0 then
      try
        Subject(PosterId(stringified.substring(0,pi).toInt),EmailAddress(stringified.substring(pi+1)))
      catch
        case NonFatal(t) =>
          throw new BadSubject("An exception occurred while trying to parse a jwt-stored Subject!", t)
    else
      throw new BadSubject("Subject is not in the expected format, should contain a '|' char.") 
case class Subject( posterId : PosterId, email : EmailAddress ):
  override def toString() : String = s"${PosterId.i(posterId)}|${EmailAddress.s(email)}"
