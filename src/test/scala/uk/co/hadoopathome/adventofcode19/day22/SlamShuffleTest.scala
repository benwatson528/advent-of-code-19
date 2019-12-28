package uk.co.hadoopathome.adventofcode19.day22

import org.scalatest.FunSuite

import scala.io.Source

class SlamShuffleTest extends FunSuite {

  test("shuffleDeck deal") {
    val instructions = parseInput("day22/deal-input.txt")
    assert(SlamShuffle.shuffleDeck(instructions, 10).mkString(" ") === "0 3 6 9 2 5 8 1 4 7")
  }

  test("shuffleDeck large") {
    val instructions = parseInput("day22/large-input.txt")
    assert(SlamShuffle.shuffleDeck(instructions, 10).mkString(" ") === "9 2 5 8 1 4 7 0 3 6")
  }

  test("shuffleDeck cut positive") {
    val instructions = Vector(CUT(3))
    assert(SlamShuffle.shuffleDeck(instructions, 10).mkString(" ") === "3 4 5 6 7 8 9 0 1 2")
  }

  test("shuffleDeck cut negative") {
    val instructions = Vector(CUT(-4))
    assert(SlamShuffle.shuffleDeck(instructions, 10).mkString(" ") === "6 7 8 9 0 1 2 3 4 5")
  }

  test("shuffleDeck real") {
    val instructions = parseInput("day22/real-input.txt")
    assert(SlamShuffle.shuffleDeck(instructions, 10007).indexOf(2019) === 3324)
  }

  private def parseInput(fileName: String): Vector[Instruction] = {
    val rawInput = Source.fromResource(fileName).getLines.toVector
    rawInput.map(mapStringToInstruction)
  }

  private def mapStringToInstruction(s: String): Instruction = s match {
    case x if x.startsWith("deal into new stack") => DEAL()
    case x if x.startsWith("cut") => CUT(x.split(" ")(1).toInt)
    case x if x.startsWith("deal with increment") => DEAL(x.split("increment ")(1).toInt)
  }
}
