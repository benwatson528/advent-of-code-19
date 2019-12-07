package uk.co.hadoopathome.adventofcode19.day07

import org.scalatest.FunSuite

import scala.io.Source

class AmplificationCircuitTest extends FunSuite {
  test("maxSignal 4,3,2,1,0") {
    val input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0".split(",").map(_.toInt).toList
    assert(AmplificationCircuit.maxSignal(input) === 43210)
  }

  test("maxSignal 0,1,2,3,4") {
    val input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0".split(",").map(_.toInt).toList
    assert(AmplificationCircuit.maxSignal(input) === 54321)
  }

  test("maxSignal 1,0,4,3,2") {
    val input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        .split(",").map(_.toInt).toList
    assert(AmplificationCircuit.maxSignal(input) === 65210)
  }

  test("sumNumbers real") {
    val input = Source.fromResource("day07/input.txt").getLines().next().split(",").map(_.toInt).toList
    assert(AmplificationCircuit.maxSignal(input) === 225056)
  }
}