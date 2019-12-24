package uk.co.hadoopathome.adventofcode19

package object day24 {

  case class Coord(x: Int, y: Int)

  type Grid = Map[Coord, Item]

  sealed abstract class Item()

  case object EMPTY extends Item

  case object BUG extends Item

  def printGrid(grid: Grid): Unit = {
    val (minX, maxX) = (grid.minBy(_._1.x)._1.x, grid.maxBy(_._1.x)._1.x)
    val (minY, maxY) = (grid.minBy(_._1.y)._1.y, grid.maxBy(_._1.y)._1.y)

    for (y <- minY to maxY) {
      println()
      for (x <- minX to maxX) {
        val product = grid(Coord(x, y))
        print(product match {
          case EMPTY => "."
          case BUG => "#"
          case _ => throw new IllegalArgumentException("Cell hasn't been explored")
        })
      }
    }
    println("\n")
  }
}
