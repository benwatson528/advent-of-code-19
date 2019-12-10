package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.Amplifier

object AmplificationCircuit {
//
//  def maxSignalSingleRun(ls: IndexedSeq[Long]): Long =
//    List(0L, 1L, 2L, 3L, 4L).permutations.map(runLinear(ls, _)).max
//
//  def maxSignalFeedbackLoop(ls: IndexedSeq[Long]): Long =
//    List(5L, 6L, 7L, 8L, 9L).permutations.map(runFeedbackLoop(ls, _)).max
//
//  private def runLinear(ls: IndexedSeq[Long], phaseSettings: List[Long]): Long = {
//    val amplifiers = phaseSettings.map(new Amplifier(ls map identity, _))
//    amplifiers.foldLeft(0L)((outputAndStatus, amp) => amp.runWithPause(outputAndStatus)._1)
//  }
//
//  private def runFeedbackLoop(ls: IndexedSeq[Long], phaseSettings: List[Long]): Long = {
//    val amplifiers = phaseSettings.map(new Amplifier(ls map identity, _))
//    runFeedbackLoopRec(amplifiers, 0)
//  }
//
//  @scala.annotation.tailrec
//  private def runFeedbackLoopRec(amplifiers: List[Amplifier], firstInput: Long): Long = {
//    val (output, isFinished) = amplifiers.foldLeft((firstInput, false))((outputAndStatus, amp) =>
//      amp.runWithPause(outputAndStatus._1))
//    if (isFinished) output else runFeedbackLoopRec(amplifiers, output)
//  }
}
