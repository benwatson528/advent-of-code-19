package uk.co.hadoopathome.adventofcode19.day18

import org.scalatest.FunSuite
import uk.co.hadoopathome.adventofcode19.day15.Grid

import scala.io.Source

class ManyWorldsInterpretationTest extends FunSuite {
  test("collectAllKeys real") {
    val input = parseInput("day18/small-input.txt")
    assert(ManyWorldsInterpretation.collectAllKeys(input) === 435)
  }

  private def parseInput(fileName: String): Maze = {
    val ls = Source.fromResource(fileName).getLines.toList
    (for (y <- ls.indices;
          x <- 0 until ls.head.length) yield (Coord(x, y) -> ls(y)(x))).toMap
  }

//  private def mapToBlock(c: Char): Block = c match {
//    case '#' => WALL
//    case '.' => EMPTY
//    case '@' => ENTRANCE
//    case x if (x.isLower) => KEY(x)
//    case x if (x.isUpper) => DOOR
//    case _ => throw new IllegalArgumentException("Invalid character found")
//  }
}
