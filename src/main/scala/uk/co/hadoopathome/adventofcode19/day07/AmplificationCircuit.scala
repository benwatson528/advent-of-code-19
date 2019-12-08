package uk.co.hadoopathome.adventofcode19.day07

import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids
import uk.co.hadoopathome.adventofcode19.day05.SunnyWithAChanceOfAsteroids.ProgramState

import scala.collection.mutable.ListBuffer

object AmplificationCircuit {
  def maxSignalSingleRun(ls: IndexedSeq[Int]): Int =
    List(0, 1, 2, 3, 4).permutations.map(runAmplifierRec(ls, 0, _)).max

  def maxSignalFeedbackLoop(ls: IndexedSeq[Int]): Int =
     List(5, 6, 7, 8, 9).permutations.toList.map(runFeedbackLoop(ls, _)).max

  private def runFeedbackLoop(ls: IndexedSeq[Int], phaseSettings: List[Int]): Int = {
    val programStates = initialiseProgramStates(ls, phaseSettings)

    var lastOutput = -1
    var i = 0
    while (true) {
      val currentProgramState = programStates(i % 5)
      val returnedProgramState = SunnyWithAChanceOfAsteroids.runProgramPause(currentProgramState)
      if (returnedProgramState.isFinished) {
        return lastOutput
      } else {
        lastOutput = returnedProgramState.output
      }
      programStates(i % 5) = returnedProgramState
      val nextProgramState = programStates((i + 1) % 5)
      programStates((i + 1) % 5) = ProgramState(nextProgramState.pointer, nextProgramState.ls,
        nextProgramState.inputs :+ returnedProgramState.output, -1, nextProgramState.isFinished)
      i = i + 1
    }
    -1
  }

  private def initialiseProgramStates(ls: IndexedSeq[Int], phaseSettings: List[Int]): ListBuffer[ProgramState] = {
    val programState1 = ProgramState(0, ls, List(phaseSettings.head, 0), -1, isFinished = false)
    val programState2 = ProgramState(0, ls, List(phaseSettings(1)), -1, isFinished = false)
    val programState3 = ProgramState(0, ls, List(phaseSettings(2)), -1, isFinished = false)
    val programState4 = ProgramState(0, ls, List(phaseSettings(3)), -1, isFinished = false)
    val programState5 = ProgramState(0, ls, List(phaseSettings(4)), -1, isFinished = false)

    var programStates = new ListBuffer[ProgramState]()
    programStates ++= ListBuffer[ProgramState](programState1, programState2, programState3, programState4, programState5)
    programStates
  }

  @scala.annotation.tailrec
  private def runAmplifierRec(input: IndexedSeq[Int], output: Int, phaseSettings: List[Int]): Int =
    phaseSettings match {
      case x :: xs => runAmplifierRec(input, SunnyWithAChanceOfAsteroids.runProgram(input, List(x, output)), xs)
      case Nil => output
    }
}
