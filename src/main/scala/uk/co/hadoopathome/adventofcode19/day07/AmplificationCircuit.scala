package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids

object AmplificationCircuit {
  def maxSignal(input: IndexedSeq[Int]): Int =
    List(0, 1, 2, 3, 4).permutations.map(runAmplifierRec(input, 0, _)).max

  @scala.annotation.tailrec
  private def runAmplifierRec(input: IndexedSeq[Int], output: Int, phaseSettings: List[Int]): Int = phaseSettings match {
    case x :: xs => runAmplifierRec(input, SunnyWithAChanceOfAsteroids.runProgram(input, List(x, output)), xs)
    case Nil => output
  }
}
