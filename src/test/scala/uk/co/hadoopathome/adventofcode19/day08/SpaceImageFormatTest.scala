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

  test("decode four layers") {
    val input = parseInput("0222112222120000")
    assert(SpaceImageFormat.decode(input, 2, 2) === List(0, 1, 1, 0))
  }

  test("decode real") {
    val input = parseInput(Source.fromResource("day08/input.txt").getLines.next())
    //printImage(SpaceImageFormat.decode(input, 25, 6), 25, 6)
  }

  private def parseInput(input: String): List[Int] = input.toString.map(_.asDigit).toList

  private def printImage(flattenedImage: List[Int], width: Int, height: Int): Unit = {
    for (y <- 0 until height) {
      println()
      for (x <- 0 until width) {
        print(if (flattenedImage((y * width) + x) == 1) "*" else " ")
      }
    }
  }
}
