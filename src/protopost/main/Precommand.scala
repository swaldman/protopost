package protopost.main

object Precommand:
  object Version extends Precommand:
    def execute() : Int =
      println( protopost.BuildInfo.version )
      0
sealed trait Precommand:
  def execute() : Int
end Precommand
