package uk.co.hadoopathome.adventofcode19.day09

import uk.co.hadoopathome.adventofcode19.day05.Intcode

object SensorBoost {
  def runIntCode(ls: IndexedSeq[Long]): Long = {
    val intcode = new Intcode(ls.toList)
    intcode.runUntilHalt()
  }

  def runIntCode(ls: IndexedSeq[Long], input: Int): Long = {
    val intcode = new Intcode(ls.toList, input)
    intcode.runUntilHalt()
  }
}
