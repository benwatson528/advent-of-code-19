package uk.co.hadoopathome.adventofcode19.day17

import uk.co.hadoopathome.adventofcode19.day05.Intcode

object SetAndForget {

  def getAlignmentParameters(ls: List[Long]): Long = {
    val grid = drawGridRec(new Intcode(ls), Map[Coord, Tile](), Coord(0, 0))
    val scaffolds = grid.filter(_._2.item == SCAFFOLD)
    val xBounds = (grid.minBy(_._1.x)._1.x, grid.maxBy(_._1.x)._1.x)
    val yBounds = (grid.minBy(_._1.y)._1.y, grid.maxBy(_._1.y)._1.y)
    sumAlignmentParamters(scaffolds, xBounds, yBounds)
  }

  @scala.annotation.tailrec
  private def drawGridRec(intcode: Intcode, grid: Map[Coord, Tile], c: Coord): Grid = {
    val (output, isFinished) = intcode.runUntilPause(None)
    if (isFinished) grid
    else {
      mapOutputToItem(output) match {
        case Some(item) =>
          val newCoord = Coord(c.x + 1, c.y)
          drawGridRec(intcode, grid + (newCoord -> Tile(newCoord, item)), newCoord)
        case None => drawGridRec(intcode, grid, Coord(0, c.y + 1))
      }
    }
  }

  private def sumAlignmentParamters(scaffolds: Map[Coord, Tile], xBounds: (Int, Int), yBounds: (Int, Int)): Long = {
    val intersectedScaffolds = scaffolds.filter(s =>
      (scaffolds.contains(s._1.copy(x = s._1.x - 1))
          && scaffolds.contains(s._1.copy(x = s._1.x + 1))
          && scaffolds.contains(s._1.copy(y = s._1.y - 1))
          && scaffolds.contains(s._1.copy(y = s._1.y + 1)))
    )
    intersectedScaffolds.foldLeft(0)((a, s) => a + (s._1.x - xBounds._1) * (s._1.y - yBounds._1))
  }

  private def mapOutputToItem(output: Long): Option[Item] = output match {
    case 35 => Some(SCAFFOLD)
    case 46 => Some(EMPTY)
    case 60 => Some(ROBOT_LEFT)
    case 62 => Some(ROBOT_RIGHT)
    case 94 => Some(ROBOT_UP)
    case 118 => Some(ROBOT_DOWN)
    case 10 => None
  }
}
