package uk.co.hadoopathome.adventofcode19.day24

import org.scalatest.FunSuite

import scala.io.Source

class PlanetOfDiscordTest extends FunSuite {

  test("findDiversityRating test input") {
    val grid = parseInput("day24/test-input.txt")
    assert(PlanetOfDiscord.findDiversityRating(grid) === 2129920)
  }

  test("findDiversityRating real input") {
    val grid = parseInput("day24/real-input.txt")
    assert(PlanetOfDiscord.findDiversityRating(grid) === 18844281)
  }

  private def parseInput(fileName: String): Grid = {
    val rawInput = Source.fromResource(fileName).getLines.toList
    (for (y <- rawInput.indices;
          x <- 0 until rawInput.head.length) yield {
      val item = rawInput(y)(x) match {
        case '.' => EMPTY
        case '#' => BUG
      }
      (Coord(x, y) -> item)
    }).toMap
  }
}
