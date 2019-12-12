package uk.co.hadoopathome.adventofcode19.day12

import org.scalatest.FunSuite

import scala.io.Source

class TheNBodyProblemTest extends FunSuite {
  test("calculateTotalEnergy small") {
    val input = parseInput("day12/small-input.txt")
    assert(TheNBodyProblem.calculateTotalEnergy(input, 10) === 179)
  }

  test("calculateTotalEnergy medium") {
    val input = parseInput("day12/medium-input.txt")
    assert(TheNBodyProblem.calculateTotalEnergy(input, 100) === 1940)
  }

  test("calculateTotalEnergy real") {
    val input = parseInput("day12/real-input.txt")
    assert(TheNBodyProblem.calculateTotalEnergy(input, 1000) === 9958)
  }

  test("findRepeatedStartState small") {
    val input = parseInput("day12/small-input.txt")
    assert(TheNBodyProblem.findRepeatedStartState(input) === 2772)
  }

  test("findRepeatedStartState large") {
    val input = parseInput("day12/medium-input.txt")
    assert(TheNBodyProblem.findRepeatedStartState(input) === 4686774924L)
  }

  test("findRepeatedStartState real") {
    val input = parseInput("day12/real-input.txt")
    assert(TheNBodyProblem.findRepeatedStartState(input) === 318382803780324L)
  }

  private def parseInput(fileName: String): List[Moon] = {
    Source.fromResource(fileName).getLines.toList.map(
      line => {
        val values = ("""-?\d*\.?\d+""".r findAllIn line).toList.map(_.toInt)
        Moon(Coord(values.head, values(1), values(2)), Coord(0, 0, 0))
      }
    )
  }
}
