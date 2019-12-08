package uk.co.hadoopathome.adventofcode19.day08

import org.scalatest.FunSuite

import scala.io.Source

class SpaceImageFormatTest extends FunSuite {
  test("checkCorruption two layers") {
    val input = parseInput("123456789012")
    assert(SpaceImageFormat.checkCorruption(input, 3, 2) === 1)
  }

  test("checkCorruption real") {
    val input = parseInput(Source.fromResource("day08/input.txt").getLines.next())
    assert(SpaceImageFormat.checkCorruption(input, 25, 6) === 2413)
  }

  private def parseInput(input: String): List[Int] = input.toString.map(_.asDigit).toList
}
