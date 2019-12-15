package uk.co.hadoopathome.adventofcode19.day15

import org.scalatest.FunSuite
import uk.co.hadoopathome.adventofcode19.day11.SpacePolice.Coord

import scala.io.Source

class OxygenSystemTest extends FunSuite {

  test("moveToOxygen real") {
    val input = Source.fromResource("day15/input.txt").getLines.next().split(",").map(_.toLong).toList
    assert(OxygenSystem.moveToOxygen(input) === 308)
  }
}
