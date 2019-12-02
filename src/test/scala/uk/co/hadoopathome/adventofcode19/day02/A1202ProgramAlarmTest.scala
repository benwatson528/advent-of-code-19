package uk.co.hadoopathome.adventofcode19.day02

import org.scalatest.FunSuite

import scala.io.Source
import scala.util.control.Breaks._

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
    assert(6327510 === A1202ProgramAlarm.runProgram(updateInput(12, 2, input)).head)
  }

  test("runProgram real part 2") {
    val input = Source.fromResource("day02/input.txt").getLines().next().split(",").toList
      .map(_.toInt).toIndexedSeq
    var foundAnswer = false
    breakable {
      for (x <- 0 to 99;
           y <- 0 to 99) {
        val finalList = A1202ProgramAlarm.runProgram(updateInput(x, y, input))
        if (19690720 == finalList.head) {
          assert(4112 === 100 * finalList(1) + finalList(2))
          foundAnswer = true
          break
        }
      }
    }
    assert(true === foundAnswer)
  }

  private def updateInput(a: Int, b: Int, ls: IndexedSeq[Int]): IndexedSeq[Int] = ls.updated(1, a).updated(2, b)
}
