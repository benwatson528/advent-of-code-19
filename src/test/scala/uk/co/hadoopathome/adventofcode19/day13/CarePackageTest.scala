package uk.co.hadoopathome.adventofcode19.day13

import org.scalatest.FunSuite

import scala.io.Source

class CarePackageTest extends FunSuite {

  test("populateBoard real") {
    val input = parseInput(Source.fromResource("day13/input.txt").getLines().next())
    val board = CarePackage.populateBoard(input)
    assert(board.count(_.id == BLOCK) === 296)
  }

  test("playGame real") {
    val input = parseInput(Source.fromResource("day13/input.txt").getLines().next())
    assert(CarePackage.playGame(input) === 13824)
  }

  private def parseInput(input: String): List[Long] = input.split(",").map(_.toLong).toList
}
