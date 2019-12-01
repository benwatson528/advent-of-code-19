package uk.co.hadoopathome.adventofcode19.day01

import org.scalatest.FunSuite

import scala.io.Source

class TheTyrannyOfTheRocketEquationTest extends FunSuite {
  test("Part 1: rounding single number") {
    val input = List(12)
    assert(2 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = false))
  }

  test("Part 1: no rounding single number") {
    val input = List(14)
    assert(2 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = false))
  }

  test("Part 1: 1969") {
    val input = List(1969)
    assert(654 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = false))
  }

  test("Part 1: 100756") {
    val input = List(100756)
    assert(33583 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = false))
  }

  test("Part 1: multiple numbers") {
    val input = List(12, 14, 1969, 100756)
    assert(34241 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = false))
  }

  test("Part 1: real") {
    val input = Source.fromResource("day01/input.txt").getLines.toList
      .map(_.toString.toInt)
    assert(3249140 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = false))
  }

  test("Part 2: rounding single number") {
    val input = List(14)
    assert(2 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = true))
  }

  test("Part 2: 1969") {
    val input = List(1969)
    assert(966 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = true))
  }

  test("Part 2: 100756") {
    val input = List(100756)
    assert(50346 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = true))
  }

  test("Part 2: multiple numbers") {
    val input = List(14, 1969, 100756)
    assert(51314 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = true))
  }

  test("Part 2: real") {
    val input = Source.fromResource("day01/input.txt").getLines.toList
      .map(_.toString.toInt)
    assert(4870838 === TheTyrannyOfTheRocketEquation.sumFuelRequirements(input, includeFuelWeight = true))
  }
}
