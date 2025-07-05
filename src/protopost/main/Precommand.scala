package protopost.main

object Precommand:
  object Version extends Precommand:
    def execute() : Int =
      println( protopost.BuildInfo.version )
      0
  object GeneratePrivateKey extends Precommand:
    def execute() : Int =
      import protopost.crypto.BouncyCastleSecp256r1
      val keypair = BouncyCastleSecp256r1.generateKeyPair()
      val S = keypair(0).getS()
      println( BouncyCastleSecp256r1.fieldValueToHex0x(S) )
      0
sealed trait Precommand:
  def execute() : Int
end Precommand
