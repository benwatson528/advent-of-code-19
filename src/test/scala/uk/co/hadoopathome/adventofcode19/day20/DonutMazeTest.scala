package uk.co.hadoopathome.adventofcode19.day20

import org.scalatest.FunSuite

import scala.io.Source

class DonutMazeTest extends FunSuite {

  test("findShortestPath small input") {
    val maze = parseInput("day20/small-input.txt")
    assert(DonutMaze.findShortestPath(maze) === 23)
  }

  test("findShortestPath large input") {
    val maze = parseInput("day20/large-input.txt")
    assert(DonutMaze.findShortestPath(maze) === 58)
  }

  test("findShortestPath real input") {
    val maze = parseInput("day20/real-input.txt")
    assert(DonutMaze.findShortestPath(maze) === 516)
  }

  private def parseInput(fileName: String): Maze = {
    val rawInput = Source.fromResource(fileName).getLines.toList
    val rawMaze = (for (y <- rawInput.indices;
                        x <- 0 until rawInput.head.length)
      yield Coord(x, y) -> rawInput(y)(x)).toMap
    rawMaze.map(extractPortals(_, rawMaze))
  }

  private def extractPortals(cell: (Coord, Char), rawMaze: Map[Coord, Char]): (Coord, Item) = {
    if (!cell._2.isLetter) {
      val item = cell._2 match {
        case '.' => PASSAGE
        case '#' => WALL
        case ' ' => EMPTY
        case _ => throw new IllegalArgumentException("Unexpected character in maze")
      }
      (cell._1, item)
    } else {
      val adjacentCells = getAdjacentCells(cell._1, rawMaze)
      adjacentCells.find(_._2 == '.') match {
        case Some(_) =>
          val secondLabel = adjacentCells.find(_._2.isLetter).get
          val portalId = if (cell._1.x < secondLabel._1.x || cell._1.y < secondLabel._1.y) 
            cell._2.toString + secondLabel._2.toString
          else
            secondLabel._2.toString + cell._2.toString
          (cell._1, PORTAL(portalId))
        case None => (cell._1, EMPTY)
      }
    }
  }

  private def getAdjacentCells(centre: Coord, rawMaze: Map[Coord, Char]): List[(Coord, Char)] = {
    List(
      (centre.copy(x = centre.x + 1), rawMaze.getOrElse(centre.copy(x = centre.x + 1), ' ')),
      (centre.copy(x = centre.x - 1), rawMaze.getOrElse(centre.copy(x = centre.x - 1), ' ')),
      (centre.copy(y = centre.y + 1), rawMaze.getOrElse(centre.copy(y = centre.y + 1), ' ')),
      (centre.copy(y = centre.y - 1), rawMaze.getOrElse(centre.copy(y = centre.y - 1), ' ')))
  }
}
