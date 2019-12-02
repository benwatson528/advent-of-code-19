package uk.co.hadoopathome.adventofcode19.day02

import org.scalatest.FunSuite

import scala.io.Source

class A1202ProgramAlarmTest extends FunSuite {
  test("runProgram opcode 1") {
    val input = IndexedSeq(1, 0, 0, 0, 99)
    assert(IndexedSeq(2, 0, 0, 0, 99) === A1202ProgramAlarm.runProgram(input))
  }

  test("runProgram opcode 2") {
    val input = IndexedSeq(2, 3, 0, 3, 99)
    assert(IndexedSeq(2, 3, 0, 6, 99) === A1202ProgramAlarm.runProgram(input))
  }

  test("runProgram end element changed") {
    val input = IndexedSeq(2, 4, 4, 5, 99, 0)
    assert(IndexedSeq(2, 4, 4, 5, 99, 9801) === A1202ProgramAlarm.runProgram(input))
  }

  test("runProgram multipe operations") {
    val input = IndexedSeq(1, 1, 1, 4, 99, 5, 6, 0, 99)
    assert(IndexedSeq(30, 1, 1, 4, 2, 5, 6, 0, 99) === A1202ProgramAlarm.runProgram(input))
  }

  test("runProgram real part 1") {
    val input = Source.fromResource("day02/input.txt").getLines().next().split(",").toList
      .map(_.toInt).toIndexedSeq
    val modifiedInput = input.updated(1, 12).updated(2, 2)
    assert(6327510 === A1202ProgramAlarm.runProgram(modifiedInput).head)
  }
}
