package uk.co.hadoopathome.adventofcode19.day01

import org.scalatest.FunSuite

import scala.io.Source

class TheTyrannyoftheRocketEquationTest extends FunSuite {
  test("sumFuelRequirements rounding single number") {
    val input = List(12)
    assert(2 === TheTyrannyoftheRocketEquation.sumFuelRequirements(input))
  }

  test("sumFuelRequirements no rounding single number") {
    val input = List(14)
    assert(2 === TheTyrannyoftheRocketEquation.sumFuelRequirements(input))
  }

  test("sumFuelRequirements 1969") {
    val input = List(1969)
    assert(654 === TheTyrannyoftheRocketEquation.sumFuelRequirements(input))
  }

  test("sumFuelRequirements 100756") {
    val input = List(100756)
    assert(33583 === TheTyrannyoftheRocketEquation.sumFuelRequirements(input))
  }

  test("sumFuelRequirements multiple numbers") {
    val input = List(12, 14, 1969, 100756)
    assert(34241 === TheTyrannyoftheRocketEquation.sumFuelRequirements(input))
  }

  test("sumFuelRequirements real") {
    val input = Source.fromResource("day01/input.txt").getLines.toList
      .map(_.toString.toInt)
    assert(3249140 === TheTyrannyoftheRocketEquation.sumFuelRequirements(input))
  }
}
