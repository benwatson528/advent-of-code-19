package uk.co.hadoopathome.adventofcode19.day20

object DonutMaze {

  val START = "AA"
  val END = "ZZ"

  def findShortestPath(maze: Maze): Int = {
    val portals = mapPortals(maze)
    val startLabel = maze.filter(_._2.isInstanceOf[PORTAL]).find(_._2.asInstanceOf[PORTAL].portalId == START).get._1
    val startCoord = findAdjacentPassage(startLabel, maze)
    traverseRec(startCoord, List[Coord](), maze, portals)
  }

  private def traverseRec(current: Coord, visited: List[Coord], maze: Maze, portals: Portals): Int =
    maze(current) match {
      case portal: PORTAL if portal.portalId == END => visited.size - 1
      case _ =>
        if (visited contains current) 10000
        else {
          maze(current) match {
            case WALL => 10000
            case EMPTY => 10000
            case PORTAL(_) =>
              if (!portals.contains(current)) 10000
              else traverseRec(portals(current).get, visited, maze, portals)
            case PASSAGE => getAdjacentCoords(current, maze).map(traverseRec(_, visited :+ current, maze, portals)).min
          }
        }
    }

  private def getAdjacentCoords(centre: Coord, maze: Maze): List[Coord] = {
    List(centre.copy(x = centre.x + 1),
      centre.copy(x = centre.x - 1),
      centre.copy(y = centre.y + 1),
      centre.copy(y = centre.y - 1))
  }

  private def mapPortals(maze: Maze): Map[Coord, Option[Coord]] = {
    val portals = maze.filter(_._2.isInstanceOf[PORTAL]).filter(_._2.asInstanceOf[PORTAL].portalId != START)
    portals.map(p => {
      val endLabel = portals.filter(_ != p)
        .find(_._2.asInstanceOf[PORTAL].portalId == p._2.asInstanceOf[PORTAL].portalId) match {
        case Some(o) =>
          Some(findAdjacentPassage(o._1, maze))
        case None => None
      }
      p._1 -> endLabel
    })
  }

  private def findAdjacentPassage(coord: Coord, maze: Maze): Coord =
    getAdjacentCoords(coord, maze).map(a => (a, maze(a))).find(_._2 == PASSAGE).get._1
}
