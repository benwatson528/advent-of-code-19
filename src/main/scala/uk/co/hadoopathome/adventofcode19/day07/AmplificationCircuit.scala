package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.Amplifier

object AmplificationCircuit {

  def maxSignalSingleRun(ls: IndexedSeq[Long]): Long =
    List(0L, 1L, 2L, 3L, 4L).permutations.map(runLinear(ls.toList, _)).max

  def maxSignalFeedbackLoop(ls: IndexedSeq[Long]): Long =
    List(5L, 6L, 7L, 8L, 9L).permutations.map(runFeedbackLoop(ls.toList, _)).max

  private def runLinear(ls: List[Long], phaseSettings: List[Long]): Long = {
    val amplifiers = phaseSettings.map(new Amplifier(_))
    amplifiers.foldLeft(0L)((outputAndStatus, amp) => amp.runWithPause(ls, outputAndStatus)._1)
  }

  private def runFeedbackLoop(ls: List[Long], phaseSettings: List[Long]): Long = {
    val amplifiers = phaseSettings.map(new Amplifier(_))
    val output = amplifiers.foldLeft(0L)((outputAndStatus, amp) => amp.runWithPause(ls, outputAndStatus)._1)
    runFeedbackLoopRec(amplifiers, output)
  }

  @scala.annotation.tailrec
  private def runFeedbackLoopRec(amplifiers: List[Amplifier], firstInput: Long): Long = {
    val (output, isFinished) = amplifiers.foldLeft((firstInput, false))((outputAndStatus, amp) =>
      amp.runWithPause(outputAndStatus._1))
    if (isFinished) output else runFeedbackLoopRec(amplifiers, output)
  }
}
