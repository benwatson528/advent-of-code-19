package uk.co.hadoopathome.adventofcode19.day05

import org.scalatest.FunSuite

import scala.io.Source

class IntcodeTest extends FunSuite {

  test("runProgram complex opcode 3 instruction") {
    val input = splitInput("3,0,4,0,99")
    assert(new Intcode(input, 1).runUntilCompletion() === 1)
  }

  test("runProgram real part 1") {
    val input = splitInput(Source.fromResource("day05/input.txt").getLines().next())
    assert(new Intcode(input, 1).runUntilCompletion() === 10987514)
  }

  test("runProgram jump (rule 5) position - output 0 if the input is 0 else 1") {
    val input = splitInput("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
    assert(new Intcode(input, 0).runUntilCompletion() === 0)
    assert(new Intcode(input, 10).runUntilCompletion() === 1)
  }

  test("runProgram jump (rule 5) immediate - output 0 if the input is 0 else 1") {
    val input = splitInput("3,3,1105,-1,9,1101,0,0,12,4,12,99,1")
    assert(new Intcode(input, 0).runUntilCompletion() === 0)
    assert(new Intcode(input, 10).runUntilCompletion() === 1)
  }

  test("runProgram less than (rule 7) position - ouputs 1 if less than 8, else 0") {
    val input = splitInput("3,9,7,9,10,9,4,9,99,-1,8")
    assert(new Intcode(input, 7).runUntilCompletion() === 1)
    assert(new Intcode(input, 9).runUntilCompletion() === 0)
  }

  test("runProgram less than (rule 7) immediate - ouputs 1 if less than 8, else 0") {
    val input = splitInput("3,3,1107,-1,8,3,4,3,99")
    assert(new Intcode(input, 7).runUntilCompletion() === 1)
    assert(new Intcode(input, 9).runUntilCompletion() === 0)
  }

  test("runProgram equals (rule 8) position comparison - ouputs 1 if equal to 8, else 0") {
    val input = splitInput("3,9,8,9,10,9,4,9,99,-1,8")
    assert(new Intcode(input, 8).runUntilCompletion() === 1)
    assert(new Intcode(input, 15).runUntilCompletion() === 0)
  }

  test("runProgram equals (rule 8) immediate comparison - ouputs 1 if equal to 8, else 0") {
    val input = splitInput("3,3,1108,-1,8,3,4,3,99")
    assert(new Intcode(input, 8).runUntilCompletion() === 1)
    assert(new Intcode(input, 15).runUntilCompletion() === 0)
  }

  test("runProgram real part 2") {
    val input = splitInput(Source.fromResource("day05/input.txt").getLines().next())
    assert(new Intcode(input, 5).runUntilCompletion() === 14195011)
  }

  private def splitInput(s: String): List[Long] = s.split(",").map(_.toLong).toList
}
