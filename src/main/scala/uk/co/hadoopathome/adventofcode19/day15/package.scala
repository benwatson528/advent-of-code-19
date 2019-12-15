package uk.co.hadoopathome.adventofcode19

package object day15 {

  case class Coord(x: Int, y: Int)
  case class CoordSteps(coords: Coord, steps: Int, manhattanDistance: Int)

  type Path = Vector[Coord]
  type Grid = Map[Coord, Block]

  sealed abstract class Block()

  case object EMPTY extends Block

  case object WALL extends Block

  case object OXYGEN extends Block

  case object UNEXPLORED extends Block

  def drawOutput(grid: Grid, currentPosition: Coord): Unit = {
    val (minX, maxX) = (grid.minBy(_._1.x)._1.x, grid.maxBy(_._1.x)._1.x)
    val (minY, maxY) = (grid.minBy(_._1.y)._1.y, grid.maxBy(_._1.y)._1.y)

    println("Highlighting " + currentPosition + " with 'x'")
    for (y <- maxY to minY by -1) {
      println()
      for (x <- minX to maxX) {
        if (x == 0 && y == 0) print("*")
        else if (x == currentPosition.x && y == currentPosition.y) print("x")
        else {
          val charToPrint = grid.getOrElse(Coord(x, y), UNEXPLORED) match {
            case EMPTY => " "
            case WALL => "█"
            case OXYGEN => "O"
            case UNEXPLORED => "█"
            case _ => throw new IllegalArgumentException("NOT A BLOCK")
          }
          print(charToPrint)
        }
      }
    }
    println("\n")
  }
}
