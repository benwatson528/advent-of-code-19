package uk.co.hadoopathome.adventofcode19.day17

import org.scalatest.FunSuite

import scala.io.Source

class SetAndForgetTest extends FunSuite {

  test("drawGrid real") {
    val input = Source.fromResource("day17/input.txt").getLines.next().split(",").map(_.toLong).toList
    assert(SetAndForget.getAlignmentParameters(input) === 4112)
  }
}
