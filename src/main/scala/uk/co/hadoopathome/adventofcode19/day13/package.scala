package uk.co.hadoopathome.adventofcode19

package object day13 {

  case class Tile(x: Int, y: Int, id: Id)

  sealed abstract class Id()

  case object EMPTY extends Id

  case object WALL extends Id

  case object BLOCK extends Id

  case object HORIZONTAL extends Id

  case object BALL extends Id

}
