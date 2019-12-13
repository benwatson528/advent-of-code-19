package uk.co.hadoopathome.adventofcode19.day11

import org.scalatest.FunSuite
import uk.co.hadoopathome.adventofcode19.day11.SpacePolice.Coord

import scala.io.Source

class SpacePoliceTest extends FunSuite {

  test("findNumPaintedPanels real") {
    val input = parseInput(Source.fromResource("day11/input.txt").getLines.next())
    assert(SpacePolice.findNumPaintedPanels(input.toList) === 1681)
  }

  test("drawRegistration real") {
    val input = parseInput(Source.fromResource("day11/input.txt").getLines.next())
    SpacePolice.drawRegistration(input.toList) //Call drawOutput() to see it
  }

  private def parseInput(input: String): IndexedSeq[Long] = input.split(",").map(_.toLong)

  def drawOutput(painted: Map[Coord, Int]): Any = {
    val (minX, maxX) = (painted.minBy(_._1.x)._1.x, painted.maxBy(_._1.x)._1.x)
    val (minY, maxY) = (painted.minBy(_._1.y)._1.y, painted.maxBy(_._1.y)._1.y)

    for (y <- maxY to minY by -1) {
      println()
      for (x <- minX to maxX) {
        print(if (painted.contains(Coord(x, y))) "█" else " ")
      }
    }
  }
}
