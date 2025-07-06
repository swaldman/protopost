package protopost.auth

import java.security.SecureRandom
import scala.collection.{immutable,mutable}

import protopost.*

object AuthManagers:
  case class Spec( version : AuthManager.BCryptVersion, longPasswordStrategy : AuthManager.LongPasswordStrategy )

  private lazy val entropy = new SecureRandom()

  val SpecsForCosts = immutable.Map (
    12 -> Spec( AuthManager.BCryptVersion.Version2A, AuthManager.LongPasswordStrategy.Strict )
  )

  val CurrentCostFactor = 12

  val CurrentAuthManager =
    val AuthManagerWithStatus( am, isCurrent ) = AuthManagerWithStatus.forCostFactor( CurrentCostFactor )
    assert( isCurrent )
    am

  object AuthManagerWithStatus:
    // MT: synchronized on this' lock
    private val memoized : mutable.HashMap[Int,AuthManagerWithStatus] = new mutable.HashMap()

    def forCostFactor( costFactor : Int ) : AuthManagerWithStatus =
      val spec = SpecsForCosts.getOrElse( costFactor, throw new UnsupportedHashedPassword("This application has never supported a cost factor of " + costFactor) )
      this.synchronized:
        memoized.getOrElseUpdate( costFactor, new AuthManagerWithStatus( new AuthManager( costFactor, spec.version, spec.longPasswordStrategy, entropy ), costFactor == CurrentCostFactor ) )

    def forHash( hash : BCryptHash ) : AuthManagerWithStatus =
      val costFactor =
        val hashArr = hash.unsafeInternalArray
        val intChars = Array( hashArr(4), hashArr(5) ) // two character human-readable cost factor in bcrypt hashes
        new String( intChars ).toInt
      forCostFactor( costFactor )
  case class AuthManagerWithStatus( authManager : AuthManager, costFactorIsCurrent : Boolean )

