package uk.co.hadoopathome.adventofcode19.day13

import org.scalatest.FunSuite

import scala.io.Source

class CarePackageTest extends FunSuite {

  test("populateTiles real") {
    val input = parseInput(Source.fromResource("day13/input.txt").getLines().next())
    assert(CarePackage.populateTiles(input).count(_.id == BLOCK) === 296)
  }

  private def parseInput(input: String): List[Long] = input.split(",").map(_.toLong).toList
}
