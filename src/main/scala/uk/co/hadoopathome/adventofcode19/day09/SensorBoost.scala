package uk.co.hadoopathome.adventofcode19.day09

import uk.co.hadoopathome.adventofcode19.day05.Amplifier

object SensorBoost {
  def runIntCode(ls: IndexedSeq[Long]): Long = {
    val amplifier = new Amplifier()
    amplifier.runUntilCompletion((ls ++ List.fill(10000000)(0L)).toList)
  }

  def runIntCode(ls: IndexedSeq[Long], input: Int): Long = {
    val amplifier = new Amplifier()
    amplifier.runUntilCompletion((ls ++ List.fill(10000000)(0L)).toList, input)
  }
}
