package uk.co.hadoopathome.adventofcode19.day04

import org.scalatest.FunSuite

import scala.io.Source

class SecureContainerTest extends FunSuite {
  test("findNumValidPasswords one consecutive") {
    val range = 123454 to 123460
    assert(SecureContainer.findNumValidPasswords(range) === 1)
  }

  test("findNumValidPasswords increasing") {
    val range = 113454 to 113460
    assert(SecureContainer.findNumValidPasswords(range) === 5)
  }

  test("findNumValidPasswords part 1 real") {
    val range = 165432 to 707912
    assert(SecureContainer.findNumValidPasswords(range) === 1716)
  }
}
