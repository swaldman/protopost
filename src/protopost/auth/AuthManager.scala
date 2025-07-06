package protopost.auth

import protopost.*

import java.security.SecureRandom

import at.favre.lib.crypto.bcrypt.{BCrypt, LongPasswordStrategy as RawLongPasswordStrategy, LongPasswordStrategies}

// from https://stackoverflow.com/questions/4443476/optimal-bcrypt-work-factor
//   Remember that the value is stored in the password: $2a$(2 chars work)$(22 chars salt)(31 chars hash). It is not a fixed value.
//
// So we can rehash on login over time to increase security!
//
// Let's keep a history of (hardcoded) AuthManagers so we can increase application security over time
//
// See AuthManagers.scala

object AuthManager:
  enum LongPasswordStrategy( val createRaw : BCrypt.Version => RawLongPasswordStrategy ):
    case HashSha512 extends LongPasswordStrategy( LongPasswordStrategies.hashSha512 )
    case Strict     extends LongPasswordStrategy( LongPasswordStrategies.strict )
    case Truncate   extends LongPasswordStrategy( LongPasswordStrategies.truncate )
  end LongPasswordStrategy

  enum BCryptVersion( val raw : BCrypt.Version ):
    case Version2A extends BCryptVersion( BCrypt.Version.VERSION_2A )
    case Version2B extends BCryptVersion( BCrypt.Version.VERSION_2B )
    case Version2X extends BCryptVersion( BCrypt.Version.VERSION_2X )
    case Version2Y extends BCryptVersion( BCrypt.Version.VERSION_2Y )

  lazy val Default = new AuthManager( 12, BCryptVersion.Version2A, LongPasswordStrategy.Strict, new SecureRandom() )

import AuthManager.*

class AuthManager( costFactor : Int, bcryptVersion : BCryptVersion, longPasswordStrategy : LongPasswordStrategy, entropy : SecureRandom ):

  lazy val hasher   : BCrypt.Hasher   = BCrypt.`with`( bcryptVersion.raw, entropy, longPasswordStrategy.createRaw( bcryptVersion.raw ) )
  lazy val verifier : BCrypt.Verifyer = BCrypt.verifyer( bcryptVersion.raw, longPasswordStrategy.createRaw( bcryptVersion.raw ) )

  def hashForPassword( password : Password ) : BCryptHash = BCryptHash( hasher.hashToChar( costFactor, password.str.toCharArray ) )

  def verifyPassword( password : Password, hash : BCryptHash ) : Boolean =
    val result = verifier.verify( password.str.toCharArray, hash.unsafeInternalArray )
    // TODO: Maybe some TRACE logging here?
    result.verified
