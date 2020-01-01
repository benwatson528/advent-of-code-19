package uk.co.hadoopathome.adventofcode19.day18

object ManyWorldsInterpretation {

  def collectAllKeys(maze: Maze): Int = {
    val entrance = maze.find(_._2 == '@').get._1
    val totalKeys = maze.count(_._2.isLower)
    moveRec(maze, entrance, List[Coord](), Set[Char](), totalKeys)
  }

  private def moveRec(maze: Maze, currentPosition: Coord, visited: List[Coord], collectedKeys: Set[Char],
                      totalKeys: Int, newKeys: Int, keyPickupPoint: Int): Int = {
    if (collectedKeys.size == totalKeys) visited.length
    else if (visited.contains())
    else {
      currentPosition match {
        case '#' => 10000
        case '.' =>
          getNextMoves(currentPosition)
              .map(moveRec(maze, _, currentPosition +: visited, collectedKeys, totalKeys))
              .min
        case x if maze(x).isLower =>
          getNextMoves(currentPosition)
              .map(moveRec(maze, _, currentPosition +: visited, collectedKeys + maze(currentPosition), totalKeys))
              .min
        case x if maze(x).isUpper =>
          if (collectedKeys.contains(maze(x))) getNextMoves(currentPosition)
              .map(moveRec(maze, _, currentPosition +: visited, collectedKeys, totalKeys))
              .min
          else 10000
      }
    }
  }

  private def getNextMoves(currentPosition: Coord) = {
    Set(currentPosition.copy(x = currentPosition.x + 1),
      currentPosition.copy(x = currentPosition.x - 1),
      currentPosition.copy(y = currentPosition.y + 1),
      currentPosition.copy(y = currentPosition.y - 1))
  }
}
