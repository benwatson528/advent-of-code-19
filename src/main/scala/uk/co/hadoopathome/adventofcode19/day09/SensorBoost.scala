package uk.co.hadoopathome.adventofcode19.day09

import uk.co.hadoopathome.adventofcode19.day05.Intcode

object SensorBoost {
  def runIntCode(ls: IndexedSeq[Long]): Long = {
    val amplifier = new Intcode(ls.toList)
    amplifier.runUntilCompletion()
  }

  def runIntCode(ls: IndexedSeq[Long], input: Int): Long = {
    val amplifier = new Intcode(ls.toList, input)
    amplifier.runUntilCompletion()
  }
}
