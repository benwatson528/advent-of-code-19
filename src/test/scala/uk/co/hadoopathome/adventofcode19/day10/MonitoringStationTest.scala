package uk.co.hadoopathome.adventofcode19.day10

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.io.Source

class MonitoringStationTest extends FunSuite {

  test("findSafestAsteroid small example") {
    val input = Source.fromResource("day10/small-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.findSafestAsteroid(grid) === (Coord(3, 4), 8))
  }

  test("findSafestAsteroid medium example") {
    val input = Source.fromResource("day10/medium-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.findSafestAsteroid(grid) === (Coord(5, 8), 33))
  }

  test("findSafestAsteroid large example") {
    val input = Source.fromResource("day10/large-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.findSafestAsteroid(grid) === (Coord(11, 13), 210))
  }

  test("findSafestAsteroid real") {
    val input = Source.fromResource("day10/real-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.findSafestAsteroid(grid)._2 === 303)
  }

  private def parseInput(input: List[String]): Grid = {
    val map = mutable.Map[Coord, Occupant]()
    for (y <- input.indices) {
      val currentRow = input(y)
      for (x <- input.head.indices) {
        map += (Coord(x, y) -> (if (currentRow(x) == '#') Asteroid else EmptySpace))
      }
    }
    map
  }

  private def printSpace(grid: Grid): Unit = {
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
