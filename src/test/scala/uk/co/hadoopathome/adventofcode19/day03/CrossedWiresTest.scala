package uk.co.hadoopathome.adventofcode19.day03

import org.scalatest.FunSuite

import scala.io.Source

class CrossedWiresTest extends FunSuite {
  test("crossWires simple") {
    val wire1 = stringToList("R8,U5,L5,D3")
    val wire2 = stringToList("U7,R6,D4,L4")
    assert(6 === CrossedWires.crossWires(wire1, wire2))
  }

  test("crossWires complex 1") {
    val wire1 = stringToList("R75,D30,R83,U83,L12,D49,R71,U7,L72")
    val wire2 = stringToList("U62,R66,U55,R34,D71,R55,D58,R83")
    assert(159 === CrossedWires.crossWires(wire1, wire2))
  }

  test("crossWires complex 2") {
    val wire1 = stringToList("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
    val wire2 = stringToList("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
    assert(135 === CrossedWires.crossWires(wire1, wire2))
  }

  test("crossWires part 1 real") {
    val input = Source.fromResource("day03/input.txt").getLines
    assert(2193 === CrossedWires.crossWires(stringToList(input.next()), stringToList(input.next())))
  }

  private def stringToList(s: String): List[String] = s.split(",").toList
}
