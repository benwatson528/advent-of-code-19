package uk.co.hadoopathome.adventofcode19.day04

import org.scalatest.FunSuite

class SecureContainerTest extends FunSuite {
  test("findNumValidPasswordsTwoAdjacent one consecutive") {
    val range = 123454 to 123460
    assert(SecureContainer.findNumValidPasswordsTwoAdjacent(range) === 1)
  }

  test("findNumValidPasswordsTwoAdjacent increasing") {
    val range = 113454 to 113460
    assert(SecureContainer.findNumValidPasswordsTwoAdjacent(range) === 5)
  }

  test("findNumValidPasswordsTwoAdjacent part 1 real") {
    val range = 165432 to 707912
    assert(SecureContainer.findNumValidPasswordsTwoAdjacent(range) === 1716)
  }

  test("findNumValidPasswordsMultipleAdjacent three consecutive") {
    val range = 123334 to 123337
    assert(SecureContainer.findNumValidPasswordsMultipleAdjacent(range) === 0)
  }

  test("findNumValidPasswordsMultipleAdjacent three consecutive with a separate two") {
    val range = 113334 to 113337
    assert(SecureContainer.findNumValidPasswordsMultipleAdjacent(range) === 4)
  }

  test("findNumValidPasswordsMultipleAdjacent part 2 real") {
    val range = 165432 to 707912
    assert(SecureContainer.findNumValidPasswordsMultipleAdjacent(range) === 1163)
  }
}
