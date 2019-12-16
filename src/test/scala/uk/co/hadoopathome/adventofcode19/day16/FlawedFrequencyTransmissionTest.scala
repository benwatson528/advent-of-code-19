package uk.co.hadoopathome.adventofcode19.day16

import org.scalatest.FunSuite

import scala.io.Source

class FlawedFrequencyTransmissionTest extends FunSuite {
  test("calculateOutput 12345678 1 phase") {
    val input = "12345678".map(_.asDigit)
    assert(FlawedFrequencyTransmission.calculateOutput(input, 1) === "48226158")
  }

  test("calculateOutput 12345678 4 phases") {
    val input = "12345678".map(_.asDigit)
    assert(FlawedFrequencyTransmission.calculateOutput(input, 4) === "01029498")
  }

  test("calculateOutput larger input 1 100 phases") {
    val input = "80871224585914546619083218645595".map(_.asDigit)
    assert(FlawedFrequencyTransmission.calculateOutput(input, 100).take(8) === "24176176")
  }

  test("calculateOutput larger input 2 100 phases") {
    val input = "19617804207202209144916044189917".map(_.asDigit)
    assert(FlawedFrequencyTransmission.calculateOutput(input, 100).take(8) === "73745418")
  }

  test("calculateOutput larger input 3 100 phases") {
    val input = "69317163492948606335995924319873".map(_.asDigit)
    assert(FlawedFrequencyTransmission.calculateOutput(input, 100).take(8) === "52432133")
  }

  test("sumNumbers real") {
    val input = Source.fromResource("day16/input.txt").getLines.next().map(_.asDigit)
    assert(FlawedFrequencyTransmission.calculateOutput(input, 100).take(8) === "53296082")
  }
}
