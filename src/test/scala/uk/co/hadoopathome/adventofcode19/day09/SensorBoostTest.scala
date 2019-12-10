package uk.co.hadoopathome.adventofcode19.day09

import org.scalatest.FunSuite

import scala.io.Source

class SensorBoostTest extends FunSuite {
  test("runIntCode copy") {
    val input = splitInput("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
    assert(SensorBoost.runIntCode(input) === 99L)
  }

  test("runIntCode 16 digit output") {
    val input = splitInput("1102,34915192,34915192,7,4,7,99,0")
    assert(SensorBoost.runIntCode(input) === 1219070632396864L)
  }

  test("runIntCode output large number in middle") {
    val input = splitInput("104,1125899906842624,99")
    assert(SensorBoost.runIntCode(input) === 1125899906842624L)
  }

  test("runIntCode malfunction check real") {
    val input = splitInput(Source.fromResource("day09/input.txt").getLines.next)
    assert(SensorBoost.runIntCode(input, 1) === 2662308295L)
  }

  test("runIntCode sensor boost small test") {
    val input = splitInput("1102,34915192,34915192,7,4,7,99,0")
    assert(SensorBoost.runIntCode(input) === 1219070632396864L)
  }

  ignore("runIntCode sensor boost real - ignored because IntelliJ throws SO (works in SBT)") {
    val input = splitInput(Source.fromResource("day09/input.txt").getLines.next)
    assert(SensorBoost.runIntCode(input, 2) === 63441L)
  }

  private def splitInput(s: String): IndexedSeq[Long] = s.split(",").map(_.toLong)
}
