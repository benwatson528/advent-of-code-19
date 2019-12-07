package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids

object AmplificationCircuit {
  def maxSignal(input: List[Int]): Int = {
    val phaseSettings = List(0, 1, 2, 3, 4).permutations
    val outputs = for (ps <- phaseSettings) yield {
      val outputAmpA = SunnyWithAChanceOfAsteroids.runProgram(input.toIndexedSeq, List(ps.head, 0))
      val outputAmpB = SunnyWithAChanceOfAsteroids.runProgram(input.toIndexedSeq, List(ps(1), outputAmpA))
      val outputAmpC = SunnyWithAChanceOfAsteroids.runProgram(input.toIndexedSeq, List(ps(2), outputAmpB))
      val outputAmpD = SunnyWithAChanceOfAsteroids.runProgram(input.toIndexedSeq, List(ps(3), outputAmpC))
      SunnyWithAChanceOfAsteroids.runProgram(input.toIndexedSeq, List(ps(4), outputAmpD))
    }
    outputs.max
  }
}
