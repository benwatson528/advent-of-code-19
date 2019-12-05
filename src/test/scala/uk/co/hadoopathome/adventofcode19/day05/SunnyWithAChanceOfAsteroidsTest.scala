package uk.co.hadoopathome.adventofcode19.day05

import org.scalatest.FunSuite

import scala.io.Source

class SunnyWithAChanceOfAsteroidsTest extends FunSuite {

  test("runProgram complex opcode 1 instruction") {
    val input = splitInput("1101,100,-1,4,0")
    assert(SunnyWithAChanceOfAsteroids.runProgram(input) === List())
  }

  test("runProgram complex opcode 2 instruction") {
    val input = splitInput("1002,4,3,4,33")
    assert(SunnyWithAChanceOfAsteroids.runProgram(input)=== List())
  }

  test("runProgram complex opcode 3 instruction") {
    val input = splitInput("3,0,4,0,99")
    assert(SunnyWithAChanceOfAsteroids.runProgram(input).last === 1)
  }

  test("runProgram real part 1") {
    val input = splitInput(Source.fromResource("day05/input.txt").getLines().next())
    assert(SunnyWithAChanceOfAsteroids.runProgram(input).last === 10987514)
  }

  private def splitInput(s: String): IndexedSeq[Int] = s.split(",").map(_.toInt)
}
