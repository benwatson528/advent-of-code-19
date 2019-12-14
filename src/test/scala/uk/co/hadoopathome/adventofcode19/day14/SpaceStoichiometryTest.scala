package uk.co.hadoopathome.adventofcode19.day14

import org.scalatest.FunSuite

import scala.io.Source

class SpaceStoichiometryTest extends FunSuite {
  test("findRequiredOre small input") {
    val input = parseInput("day14/small-input.txt")
    assert(SpaceStoichiometry.findRequiredOre(input) === 31)
  }

  test("findRequiredOre medium input") {
    val input = parseInput("day14/medium-input.txt")
    assert(SpaceStoichiometry.findRequiredOre(input) === 165)
  }

  test("findRequiredOre large input 1") {
    val input = parseInput("day14/large-input-1.txt")
    assert(SpaceStoichiometry.findRequiredOre(input) === 13312)
  }

  test("findRequiredOre large input 2") {
    val input = parseInput("day14/large-input-2.txt")
    assert(SpaceStoichiometry.findRequiredOre(input) === 180697)
  }

  test("findRequiredOre large input 3") {
    val input = parseInput("day14/large-input-3.txt")
    assert(SpaceStoichiometry.findRequiredOre(input) === 2210736)
  }

  test("findRequiredOre real") {
    val input = parseInput("day14/real-input.txt")
    assert(SpaceStoichiometry.findRequiredOre(input) === 435) // Not 8949517829
  }

  private def parseInput(fileName: String): Vector[Equation] =
    Source.fromResource(fileName).getLines.toList.map(parseRow).toVector

  private def parseRow(s: String): Equation = {
    val (rawLhs, rawRhs) = (s.split(" => ")(0), s.split(" => ")(1))
    val rhs = (rawRhs.split(" ")(1), rawRhs.split(" ")(0).toLong)
    val lhs = rawLhs.split(", ").map(x => x.split(" ")(1) -> x.split(" ")(0).toLong).toMap
    Equation(lhs, rhs)
  }
}
