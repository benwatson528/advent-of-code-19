package uk.co.hadoopathome.adventofcode19

package object day17 {

  case class Coord(x: Int, y: Int)

  case class Tile(coord: Coord, item: Item)

  type Grid = Map[Coord, Tile]

  sealed abstract class Item()

  case object EMPTY extends Item

  case object ROBOT_UP extends Item

  case object ROBOT_DOWN extends Item

  case object ROBOT_LEFT extends Item

  case object ROBOT_RIGHT extends Item

  case object SCAFFOLD extends Item

  def printGrid(grid: Grid): Unit = {
    val (minX, maxX) = (grid.minBy(_._1.x)._1.x, grid.maxBy(_._1.x)._1.x)
    val (minY, maxY) = (grid.minBy(_._1.y)._1.y, grid.maxBy(_._1.y)._1.y)

    for (y <- maxY to minY by -1) {
      println()
      for (x <- minX to maxX) {
        val product = grid(Coord(x, y)).item
        print(product match {
          case EMPTY => "."
          case SCAFFOLD => "#"
          case ROBOT_UP => "^"
          case ROBOT_DOWN => "v"
          case ROBOT_LEFT => "<"
          case ROBOT_RIGHT => ">"
          case _ => throw new IllegalArgumentException("Cell hasn't been explored")
        })
      }
    }
    println("\n")
  }
}
