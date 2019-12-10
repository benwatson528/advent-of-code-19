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
    assert(MonitoringStation.findSafestAsteroid(grid) === (Coord(26, 29), 303))
  }

  test("vaporiseAsteroids small example") {
    val input = Source.fromResource("day10/vaporise-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.vaporiseAsteroids(grid, Coord(8, 3), 18) === (Coord(4, 4)))
  }

  test("vaporiseAsteroids large example") {
    val input = Source.fromResource("day10/large-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.vaporiseAsteroids(grid, Coord(11, 13), 200) === (Coord(8, 2)))
  }

  test("vaporiseAsteroids real") {
    val input = Source.fromResource("day10/real-input.txt").getLines.toList
    val grid = parseInput(input)
    assert(MonitoringStation.vaporiseAsteroids(grid, Coord(26, 29), 200) === (Coord(4, 8)))
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
}
