package uk.co.hadoopathome.adventofcode19.day24

object PlanetOfDiscord {

  def findDiversityRating(grid: Grid): Long = {
    val repeatedGrid = evolveGridRec(grid, Set(grid))
    calculateDiversityRating(repeatedGrid)
  }

  @scala.annotation.tailrec
  private def evolveGridRec(grid: Grid, previousStates: Set[Grid]): Grid = {
    val updatedGrid = grid.map(c => (c._1, updateCell(grid, c)))
    if (previousStates contains updatedGrid) updatedGrid
    else evolveGridRec(updatedGrid, previousStates + updatedGrid)
  }

  private def updateCell(grid: Grid, cell: (Coord, Item)): Item = {
    getNumAdjacentBugs(cell._1, grid) match {
      case x if cell._2 == BUG && x != 1 => EMPTY
      case x if cell._2 == EMPTY && (x == 1 || x == 2) => BUG
      case _ => cell._2
    }
  }

  private def calculateDiversityRating(grid: Grid): Long = {
    val gridWidth = grid.maxBy(_._1.x)._1.x + 1
    val updated = grid.map(c => (c._1,
      c._2 match {
        case BUG =>
          val cellNumber = (c._1.y * gridWidth) + c._1.x
          math.pow(2, cellNumber).toLong
        case EMPTY => 0L
      }))
    updated.values.sum
  }

  private def getNumAdjacentBugs(coord: Coord, grid: Grid): Int = {
    List(
      grid.getOrElse(coord.copy(x = coord.x + 1), EMPTY),
      grid.getOrElse(coord.copy(x = coord.x - 1), EMPTY),
      grid.getOrElse(coord.copy(y = coord.y + 1), EMPTY),
      grid.getOrElse(coord.copy(y = coord.y - 1), EMPTY))
      .count(_ == BUG)
  }
}
