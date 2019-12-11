package uk.co.hadoopathome.adventofcode19.day11

import org.scalatest.FunSuite

import scala.io.Source

class SpacePoliceTest extends FunSuite {

  test("findNumPaintedPanels real") {
    val input = parseInput(Source.fromResource("day11/input.txt").getLines.next())
    assert(SpacePolice.findNumPaintedPanels(input.toList) === 1681)
  }

//  test("drawRegistration real") {
//    val input = parseInput(Source.fromResource("day11/input.txt").getLines.next())
//    assert(SpacePolice.drawRegistration(input.toList) === 1681)
//  }

  private def parseInput(input: String): IndexedSeq[Long] = input.split(",").map(_.toLong)

  private def drawOutput(): Unit = {

  }
}
