package uk.co.hadoopathome.adventofcode19.day07

import org.scalatest.FunSuite

import scala.io.Source

//Doesn't work after day 13 - need a way to provide multiple inputs
class AmplificationCircuitTest(ignore: String) extends FunSuite {
  test("maxSignalSingleLoop 4,3,2,1,0") {
    val input = parseInput("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
    assert(AmplificationCircuit.maxSignalSingleRun(input) === 43210)
  }

  test("maxSignalSingleLoop 0,1,2,3,4") {
    val input = parseInput("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
    assert(AmplificationCircuit.maxSignalSingleRun(input) === 54321)
  }

  test("maxSignalSingleLoop 1,0,4,3,2") {
    val input = parseInput("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
    assert(AmplificationCircuit.maxSignalSingleRun(input) === 65210)
  }

  test("maxSignalSingleLoop real") {
    val input = parseInput(Source.fromResource("day07/input.txt").getLines().next())
    assert(AmplificationCircuit.maxSignalSingleRun(input) === 225056)
  }

  test("maxSignalFeedbackLoop 9,8,7,6,5") {
    val input = parseInput("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
    assert(AmplificationCircuit.maxSignalFeedbackLoop(input) === 139629729)
  }

  test("maxSignalFeedbackLoop 9,7,8,5,6") {
    val input = parseInput("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1," +
        "53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
    assert(AmplificationCircuit.maxSignalFeedbackLoop(input) === 18216)
  }

  test("maxSignalFeedbackLoop real") {
    val input = parseInput(Source.fromResource("day07/input.txt").getLines().next())
    assert(AmplificationCircuit.maxSignalFeedbackLoop(input) === 14260332)
  }

  private def parseInput(input: String): IndexedSeq[Long] = input.split(",").map(_.toLong)
}