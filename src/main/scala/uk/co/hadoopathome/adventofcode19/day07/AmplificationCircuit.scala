package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids

object AmplificationCircuit {
  def maxSignalSingleRun(input: IndexedSeq[Int]): Int =
    List(0, 1, 2, 3, 4).permutations.map(runAmplifierRec(input, 0, _)).max

  def maxSignalFeedbackLoop(input: IndexedSeq[Int]): Int =
    runFeedbackAmplifierRec(input, 0, List(9, 8, 7, 6, 5))

  @scala.annotation.tailrec
  private def runAmplifierRec(input: IndexedSeq[Int], output: Int, phaseSettings: List[Int]): Int =
    phaseSettings match {
    case x :: xs => runAmplifierRec(input, SunnyWithAChanceOfAsteroids.runProgram(input, List(x, output)), xs)
    case Nil => output
  }

  @scala.annotation.tailrec
  private def runFeedbackAmplifierRec(input: IndexedSeq[Int], output: Int, phaseSettings: List[Int]): Int =
    phaseSettings match {
      case x :: xs => runFeedbackAmplifierRec(input, SunnyWithAChanceOfAsteroids.runProgram(input, List(x, output)), xs)
      case Nil => output
    }
}
