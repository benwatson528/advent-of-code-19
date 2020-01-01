package uk.co.hadoopathome.adventofcode19

package object day18 {

  case class Coord(x: Int, y: Int)

  sealed abstract class Block()

  case object EMPTY extends Block

  case object ENTRANCE extends Block

  case object KEY extends Block

  case object DOOR extends Block

  case object WALL extends Block

  type Maze = Map[Coord, Char]
}
