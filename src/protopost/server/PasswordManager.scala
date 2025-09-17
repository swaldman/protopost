/*

package protopost

import com.mchange.pimento

object PasswordManager:
  object Password:
    private[protopost] inline def apply( arr : Array[Char] ) : Password = arr
    private[protopost] inline def apply( s : String )        : Password = s.toCharArray()
  opaque type Password = Array[Char]

  extension( password : Password )
    @targetName("passwordUnsafeInternalArray") private[protopost] inline def unsafeInternalArray : Array[Char] = password

  object BCryptHash:
    private[protopost] def apply( chars : Array[Char] ) : BCryptHash =
      require( chars.length == 60, "A BCrypt salted hash must contain precisely 60 characters, provided hash contains " + chars.length )
      chars
  opaque type BCryptHash = Array[Char]

  extension( bchash : BCryptHash )
    @targetName("bcryptHashUnsafeInternalArray") private[protopost] inline def unsafeInternalArray : Array[Char] = bchash

  given bcryptHashIsErasable : pimento.Erasable[BCryptHash] with
    def erase( thing : BCryptHash ) = pimento.Erasable.eraseCharArray( thing.unsafeInternalArray )

  given passwordIsErasable : pimento.Erasable[Password] with
    def erase( thing : Password ) = pimento.Erasable.eraseCharArray( thing.unsafeInternalArray )

import PasswordManager.*

class PasswordManager( conn : Connection, pgdb : PgDatabase ) extends pimento.PasswordManager.FavreBCrypt[Password,BCryptHash]:
  def createStorableCredential( password : Password ) : BCryptHash =
    hasher.hashToChar( costFactor, password.unsafeInternalArray )
  def checkAgainstCredential( password : Array[Char], storedCredential : Array[Char] ) : Boolean =
    verifier.verify( password, storedCredential ).verified

*/

