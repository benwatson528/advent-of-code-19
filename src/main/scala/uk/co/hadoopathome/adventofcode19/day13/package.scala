package uk.co.hadoopathome.adventofcode19

package object day13 {

  sealed abstract class Command()

  case class Score(x: Int, y: Int, score: Long) extends Command

  case class Tile(x: Int, y: Int, id: Id) extends Command

  sealed abstract class Id()

  case object EMPTY extends Id

  case object WALL extends Id

  case object BLOCK extends Id

  case object PADDLE extends Id

  case object BALL extends Id

}
