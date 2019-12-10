package uk.co.hadoopathome.adventofcode19

import scala.collection.mutable

package object day10 {

  case class Coord(x: Int, y: Int)

  sealed trait Occupant {}

  object Asteroid extends Occupant

  object EmptySpace extends Occupant

  type Grid = mutable.Map[Coord, Occupant]

  def printSpace(grid: Grid): Unit = {
    val width = grid.maxBy(p => p._1.x)._1.x
    val height = grid.maxBy(p => p._1.y)._1.y

    for (y <- 0 to height) {
      println()
      for (x <- 0 to width) {
        print(if (grid(Coord(x, y)) == Asteroid) "#" else ".")
      }
    }
  }
}
