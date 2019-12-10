package uk.co.hadoopathome.adventofcode19.day05

import scala.collection.mutable.ListBuffer

class Amplifier(var ls: IndexedSeq[Long], phaseSetting: List[Long] = List[Long]()) {

  def this(ls: IndexedSeq[Long], phaseSetting: Long) {
    this(ls, List[Long](phaseSetting))
  }

  case class ProgramState(pointer: Int, inputs: List[Long], isFinished: Boolean)

  private case class Instruction(opcode: Long, immediateModes: Set[Int], relativeModes: Set[Int])

  private val outputs = new ListBuffer[Long]()
  private var lastProgramState = ProgramState(0, List[Long](), isFinished = false)
  private var relativeBase = 0L

  def runUntilCompletion(input: Long): Long = runUntilOutputRec(ProgramState(0, phaseSetting :+ input,
    isFinished = false))

  def runUntilCompletion(): Long = runUntilOutputRec(ProgramState(0, phaseSetting, isFinished = false))

  def runWithPause(input: Long): (Long, Boolean) = {
    lastProgramState = iterateProgramRec(lastProgramState.pointer, phaseSetting :+ input)
    (outputs.last, lastProgramState.isFinished)
  }

  @scala.annotation.tailrec
  private def runUntilOutputRec(programState: ProgramState): Long = {
    if (programState.isFinished) {
      print("OUTPUTS = " + outputs)
      outputs.last
    }
    else
      runUntilOutputRec(iterateProgramRec(programState.pointer, programState.inputs))
  }

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, inputs: List[Long]): ProgramState = {
    val instruction = parseInstruction(ls(i))
    instruction.opcode match {
      case 1 =>
        operate(i, add, instruction)
        iterateProgramRec(i + 4, inputs)
      case 2 =>
        operate(i, mult, instruction)
        iterateProgramRec(i + 4, inputs)
      case 3 =>
        writeValue(i + 1, inputs.head, instruction)
        iterateProgramRec(i + 2, inputs.tail)
      case 4 =>
        outputs += readValues(1, i, instruction).head
        ProgramState(i + 2, inputs, isFinished = false)
      case 5 => iterateProgramRec(jump(i, isJumpIfTrue = true, instruction).toInt, inputs)
      case 6 => iterateProgramRec(jump(i, isJumpIfTrue = false, instruction).toInt, inputs)
      case 7 => checkEquality(i, lt, instruction, inputs)
      case 8 => checkEquality(i, eq, instruction, inputs)
      case 9 =>
        relativeBase = relativeBase + readValues(1, i, instruction).head
        iterateProgramRec(i + 2, inputs)
      case 99 => ProgramState(0, List[Long](), isFinished = true)
    }
  }

  private def parseInstruction(i: Long): Instruction = {
    val rawInstruction = i.toString
    val immediateModes = for (i <- rawInstruction.length - 2 to 0 by -1; if (rawInstruction(i) == '1'))
      yield rawInstruction.length - 3 - i
    val relativeModes = for (i <- rawInstruction.length - 2 to 0 by -1; if (rawInstruction(i) == '2'))
      yield rawInstruction.length - 3 - i
    Instruction(i % 100, immediateModes.toSet, relativeModes.toSet)
  }

  private def operate(startIndex: Int, fn: (Long, Long) => Long, instruction: Instruction): Unit = {
    val values = readValues(2, startIndex, instruction)
     writeValue(startIndex + 3, fn(values(0), values(1)), instruction)
  }

  private def jump(startIndex: Int, isJumpIfTrue: Boolean, instruction: Instruction): Long = {
    val values = readValues(2, startIndex, instruction)
    val comparison = if (isJumpIfTrue) values(0) != 0 else values(0) == 0
    if (comparison) values(1) else startIndex + 3
  }

  private def checkEquality(startIndex: Int, fn: (Long, Long) => Boolean, instruction: Instruction,
                            inputs: List[Long]): ProgramState = {
    val values = readValues(2, startIndex, instruction)
    val comparison = if (fn(values(0), values(1))) 1 else 0
    writeValue(startIndex + 3, comparison, instruction)
    iterateProgramRec(startIndex + 4, inputs)
  }

  private def readValues(numValues: Int, startIndex: Int, instruction: Instruction): IndexedSeq[Long] = {
    for (i <- 0 until numValues) yield {
      val rootIndex = startIndex + i + 1
      if (instruction.immediateModes.contains(i))
        ls(rootIndex)
      else if (instruction.relativeModes.contains(i))
        ls(ls(rootIndex.toInt).toInt + relativeBase.toInt).toInt
      else
        ls(ls(rootIndex).toInt)
    }
  }

  private def writeValue(index: Int, value: Long, instruction: Instruction): Unit = {
    if (instruction.relativeModes.contains(index - 1))
      ls = ls.updated(ls(index).toInt + relativeBase.toInt, value)
    else
      ls = ls.updated(ls(index).toInt, value)
  }

  private val add = (a: Long, b: Long) => a + b
  private val mult = (a: Long, b: Long) => a * b
  private val lt = (a: Long, b: Long) => a < b
  private val eq = (a: Long, b: Long) => a == b
}
