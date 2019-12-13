package uk.co.hadoopathome.adventofcode19.day13

import org.scalatest.FunSuite

import scala.io.Source

class CarePackageTest extends FunSuite {

  test("populateTiles real") {
    val input = parseInput(Source.fromResource("day13/input.txt").getLines().next())
    val tiles = CarePackage.populateTiles(input)
    printGame(tiles)
    assert(tiles.count(_.id == BLOCK) === 296)
  }

//  test("playGame real") {
//    val input = parseInput(Source.fromResource("day13/input.txt").getLines().next())
//    CarePackage.playGame(input)
//  }

  private def parseInput(input: String): List[Long] = input.split(",").map(_.toLong).toList

  private def printGame(tiles: List[Tile]): Unit = {
    val (minX, maxX) = (tiles.minBy(_.x).x, tiles.maxBy(_.x).x)
    val (minY, maxY) = (tiles.minBy(_.y).y, tiles.maxBy(_.y).y)

    for (y <- minY to maxY) {
      println()
      for (x <- minX to maxX) {
        val tileAtPosition = tiles.find(t => t.x == x && t.y == y)
        tileAtPosition match {
          case Some(t) if t.id == EMPTY => print(" ")
          case Some(t) if t.id == WALL => print("w")
          case Some(t) if t.id == BLOCK => print("b")
          case Some(t) if t.id == HORIZONTAL => print("=")
          case Some(t) if t.id == BALL => print("*")
        }
      }
    }
  }
}
