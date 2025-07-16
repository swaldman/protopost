package protopost.auth

import java.security.SecureRandom
import scala.collection.{immutable,mutable}

import protopost.*

object AuthManagers:
  object Spec:
    def apply( bch : BCryptHash ) : Spec = Spec( version( bch ), costFactor( bch ) )
  case class Spec( version : AuthManager.BCryptVersion, costFactor : Int )

  private lazy val entropy = new SecureRandom()

  val LongPasswordStrategyForSpec = immutable.Map (
    Spec( AuthManager.BCryptVersion.Version2A, 12 ) -> AuthManager.LongPasswordStrategy.Strict
  )

  val CurrentSpec = Spec( AuthManager.BCryptVersion.Version2A, 12 )

  val CurrentAuthManager =
    val AuthManagerWithStatus( am, isCurrent ) = AuthManagerWithStatus.forSpec( CurrentSpec )
    assert( isCurrent )
    am

  def costFactor( bch : BCryptHash ) : Int =
    val hashArr = bch.unsafeInternalArray
    val intChars = Array( hashArr(4), hashArr(5) ) // two character human-readable cost factor in bcrypt hashes
    new String( intChars ).toInt

  def version( bch : BCryptHash ) : AuthManager.BCryptVersion =
    val hashArr = bch.unsafeInternalArray
    val tagChars = Array( hashArr(1), hashArr(2) ) // two character human-readable cost factor in bcrypt hashes
    val tag = new String( tagChars )
    AuthManager.BCryptVersion.forTag( tag ).getOrElse( throw new UnsupportedHashedPassword("Unknown BCrypt version: " + tag) )

  object AuthManagerWithStatus:
    // MT: synchronized on this' lock
    private val memoized : mutable.HashMap[Spec,AuthManagerWithStatus] = new mutable.HashMap()

    def forSpec( spec : Spec ) : AuthManagerWithStatus =
      val lps = LongPasswordStrategyForSpec.getOrElse( spec, throw new UnsupportedHashedPassword("This application has never supported bcrypt hashes of type " + spec) )
      this.synchronized:
        memoized.getOrElseUpdate( spec, new AuthManagerWithStatus( new AuthManager( spec.costFactor, spec.version, lps, entropy ), spec == CurrentSpec ) )

    def forHash( hash : BCryptHash ) : AuthManagerWithStatus = 
      forSpec( Spec(hash) )
  case class AuthManagerWithStatus( authManager : AuthManager, isCurrent : Boolean )

  def initialNewPasswordHash( pid : PosterId, password : Password ) : Unit =
    CurrentAuthManager.hashForPassword(password)

  def overwriteNewPassword( pid : PosterId, password : Password, storeHash : (PosterId, BCryptHash) => Unit ) : Unit =
    val bchash = CurrentAuthManager.hashForPassword(password)
    storeHash( pid, bchash )

  def verifyRehash( pid : PosterId, password : Password, fetchHash : PosterId => BCryptHash, storeHash : (PosterId, BCryptHash) => Unit ) : Boolean =
    val hash = fetchHash( pid )
    val amws = AuthManagerWithStatus.forHash(hash)
    val out = amws.authManager.verifyPassword( password, hash )
    if !amws.isCurrent then storeHash( pid, CurrentAuthManager.hashForPassword( password ) )
    out
