package uk.co.hadoopathome.adventofcode19.day06

import org.scalatest.FunSuite

import scala.io.Source

class UniversalOrbitMapTest extends FunSuite {
  test("countOrbits test") {
    val input = "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L".split(" ").toList
    assert(UniversalOrbitMap.countOrbits(input) === 42)
  }

  test("countOrbits part 1 real") {
    val input = Source.fromResource("day06/input.txt").getLines.toList
    assert(UniversalOrbitMap.countOrbits(input) === 117672)
  }

  test("minimumTravel test") {
    val input = "COM)B B)C C)D D)E E)F B)G G)H D)I E)J J)K K)L K)YOU I)SAN".split(" ").toList
    assert(UniversalOrbitMap.minimumTravel(input, "YOU", "SAN") === 4)
  }

  test("minimumTravel part 2 real") {
    val input = Source.fromResource("day06/input.txt").getLines.toList
    assert(UniversalOrbitMap.minimumTravel(input, "YOU", "SAN") === 277)
  }
}


