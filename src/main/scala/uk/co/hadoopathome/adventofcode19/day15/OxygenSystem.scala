package uk.co.hadoopathome.adventofcode19.day15

import uk.co.hadoopathome.adventofcode19.day05.Intcode

import scala.collection.mutable

object OxygenSystem {

  private val START_POSITION = Coord(0, 0)

  def moveToOxygen(ls: List[Long]): Int = {
    val intcode = new Intcode(ls)
    val grid = exploreBoardRec(intcode, Vector[Coord](), START_POSITION, Map[Coord, Block]())
    val oxygen = grid.find(_._2 == OXYGEN).get._1
    findShortestPath(oxygen, grid, START_POSITION)
  }

  def furthestPointForOxygen(ls: List[Long]): Int = {
    val intcode = new Intcode(ls)
    val grid = exploreBoardRec(intcode, Vector[Coord](), START_POSITION, Map[Coord, Block]())
    val oxygen = grid.find(_._2 == OXYGEN).get._1
    val startPoints = grid.filter(_._2 == EMPTY)
    startPoints.map(p => findShortestPath(oxygen, grid, p._1)).max
  }

  private def findShortestPath(oxygen: Coord, grid: Grid, startPosition: Coord): Int = {
    val initialCoordSteps = CoordSteps(startPosition, 0, heuristic(startPosition, oxygen))
    val priorityQueue = new mutable.PriorityQueue[CoordSteps]()(Ordering.by(-_.manhattanDistance))
    priorityQueue.enqueue(initialCoordSteps)
    findShortestPathRec(priorityQueue, List[Coord](), oxygen, grid)
  }

  @scala.annotation.tailrec
  private def findShortestPathRec(queue: mutable.PriorityQueue[CoordSteps], visited: List[Coord], oxygen: Coord,
                                  grid: Grid): Int = {
    val elem = queue.dequeue()
    if (elem.coords != oxygen) {
      findShortestPathRec(enqueueAdjacentCoords(queue, elem, visited, oxygen, grid),
        findAdjacentCoords(elem.coords) ::: visited, oxygen, grid)
    } else {
      elem.steps
    }
  }

  private def findAdjacentCoords(coord: Coord): List[Coord] = {
    var ls = List[Coord]()
    for (i <- 1 to 4) {
      ls = moveInDirection(coord, i) :: ls
    }
    ls
  }

  private def enqueueAdjacentCoords(q: mutable.PriorityQueue[CoordSteps], c: CoordSteps, visited: List[Coord],
                                    oxygen: Coord, grid: Grid): mutable.PriorityQueue[CoordSteps] = {
    for (adj <- findAdjacentCoords(c.coords)) {
      if (!visited.contains(adj) && grid(adj) != WALL) {
        q.enqueue(CoordSteps(adj, c.steps + 1, heuristic(adj, oxygen)))
      }
    }
    q
  }

  private def heuristic(currentCoord: Coord, oxygen: Coord): Int =
    math.abs(currentCoord.x - oxygen.x) + math.abs(currentCoord.y - oxygen.y)

  @scala.annotation.tailrec
  private def exploreBoardRec(intcode: Intcode, currentPath: Path, currentPosition: Coord, history: Grid): Grid = {
    val newDirection = directionToMoveIn(intcode, currentPath, history, currentPosition)
    if (newDirection.isEmpty) {
      if (currentPath.isEmpty) return history
      val directionToRegress = moveToOtherCoord(currentPosition, currentPath.head)
      intcode.runUntilPause(Some(directionToRegress))._1
      exploreBoardRec(intcode, currentPath.tail, currentPath.head, history)
    } else {
      val (movedOutcome, _) = intcode.runUntilPause(Some(newDirection.get.toLong))
      val cellMovedTo = moveInDirection(currentPosition, newDirection.get)
      movedOutcome match {
        case 0 => exploreBoardRec(intcode, currentPath, currentPosition, history + (cellMovedTo -> WALL))
        case 1 =>
          exploreBoardRec(intcode, currentPosition +: currentPath, cellMovedTo, history + (cellMovedTo -> EMPTY))
        case 2 =>
          exploreBoardRec(intcode, currentPosition +: currentPath, cellMovedTo, history + (cellMovedTo -> OXYGEN))
      }
    }
  }

  private def directionToMoveIn(intcode: Intcode, visited: Path, history: Grid, currentPosition: Coord): Option[Int] =
    (1 to 4).find(i => !history.contains(moveInDirection(currentPosition, i)))

  private def moveInDirection(coord: Coord, direction: Int): Coord = direction match {
    case 1 => coord.copy(y = coord.y + 1)
    case 2 => coord.copy(y = coord.y - 1)
    case 3 => coord.copy(x = coord.x - 1)
    case 4 => coord.copy(x = coord.x + 1)
  }

  private def moveToOtherCoord(currentPosition: Coord, positionToGetTo: Coord): Int = {
    if (currentPosition.x < positionToGetTo.x) 4
    else if (currentPosition.x > positionToGetTo.x) 3
    else if (currentPosition.y < positionToGetTo.y) 1
    else if (currentPosition.y > positionToGetTo.y) 2
    else throw new IllegalArgumentException("Can't move backwards to the same spot")
  }
}
