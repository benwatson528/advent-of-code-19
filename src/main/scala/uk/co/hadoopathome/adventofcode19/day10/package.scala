package uk.co.hadoopathome.adventofcode19

import scala.collection.mutable

package object day10 {

  case class Coord(x: Int, y: Int)

  sealed trait Occupant {}

  object Asteroid extends Occupant

  object EmptySpace extends Occupant

  type Grid = mutable.Map[Coord, Occupant]
}
