package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids
import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids.ProgramState

import scala.collection.mutable
import scala.collection.mutable.Queue

object AmplificationCircuit {
  def maxSignalSingleRun(ls: IndexedSeq[Int]): Int = List(0, 1, 2, 3, 4).permutations.map(runAmplifierRec(ls, 0, _)).max

  def maxSignalFeedbackLoop(ls: IndexedSeq[Int]): Int =
    List(5, 6, 7, 8, 9).permutations.toList.map(runFeedbackLoop(ls, _)).max



  private def runFeedbackLoop(ls: IndexedSeq[Int], phaseSettings: List[Int]): Int = {
    val programStates = initialiseProgramStates(ls, phaseSettings)
    var lastOutput = 0
    while (true) {
      val nextProgramState = programStates.dequeue
      val nextProgramStateWithInput = ProgramState(nextProgramState.pointer, nextProgramState.ls,
        nextProgramState.inputs :+ lastOutput, None, nextProgramState.isFinished)
      val returnedProgramState = SunnyWithAChanceOfAsteroids.runProgramPause(nextProgramStateWithInput)
      if (returnedProgramState.isFinished) {
        return lastOutput
      } else {
        lastOutput = returnedProgramState.output.get
      }
      programStates.enqueue(returnedProgramState)
    }
    -1
  }

  private def initialiseProgramStates(ls: IndexedSeq[Int], phaseSettings: List[Int]): mutable.Queue[ProgramState] = {
  //  mutable.Queue[ProgramState](ProgramState(0, ls, List(phaseSettings.head, 0), None, isFinished = false))
    new mutable.Queue[ProgramState].enqueueAll(phaseSettings.map(ps => ProgramState(0, ls, List(ps), None, isFinished = false)))
  }

  @scala.annotation.tailrec
  private def runAmplifierRec(input: IndexedSeq[Int], output: Int, phaseSettings: List[Int]): Int =
    phaseSettings match {
      case x :: xs => runAmplifierRec(input, SunnyWithAChanceOfAsteroids.runProgram(input, List(x, output)), xs)
      case Nil => output
    }
}
