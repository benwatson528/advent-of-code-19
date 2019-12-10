package uk.co.hadoopathome.adventofcode19.day05

import scala.collection.mutable.ListBuffer

class Amplifier(initialInputs: List[Long] = List[Long]()) {

  def this(initialInput: Long) {
    this(List[Long](initialInput))
  }

  case class ProgramState(pointer: Int, ls: Map[Int, Long], inputs: List[Long], isFinished: Boolean)

  private case class Instruction(opcode: Long, immediateModes: Set[Int], relativeModes: Set[Int])

  private val outputs = new ListBuffer[Long]()
  private var lastProgramState = ProgramState(0, Map[Int, Long](), List[Long](), isFinished = false)
  private var relativeBase = 0L

  def runUntilCompletion(ls: List[Long], input: Long): Long = runUntilOutputRec(ProgramState(0, convertInputToMap(ls),
    initialInputs :+ input,
    isFinished = false))

  def runUntilCompletion(ls: List[Long]): Long = runUntilOutputRec(ProgramState(0, convertInputToMap(ls), initialInputs,
    isFinished = false))

  def runWithPause(ls: List[Long], input: Long): (Long, Boolean) = {
    lastProgramState = iterateProgramRec(0, convertInputToMap(ls), initialInputs :+ input)
    (outputs.last, lastProgramState.isFinished)
  }

  def runWithPause(input: Long): (Long, Boolean) = {
    lastProgramState = iterateProgramRec(lastProgramState.pointer, lastProgramState.ls, lastProgramState.inputs :+ input)
    (outputs.last, lastProgramState.isFinished)
  }

  @scala.annotation.tailrec
  private def runUntilOutputRec(programState: ProgramState): Long = {
    if (programState.isFinished) outputs.last
    else runUntilOutputRec(iterateProgramRec(programState.pointer, programState.ls, programState.inputs))
  }

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, ls: Map[Int, Long], inputs: List[Long]): ProgramState = {
    val instruction = parseInstruction(ls(i))
    instruction.opcode match {
      case 1 => iterateProgramRec(i + 4, operate(i, ls, add, instruction), inputs)
      case 2 => iterateProgramRec(i + 4, operate(i, ls, mult, instruction), inputs)
      case 3 => iterateProgramRec(i + 2, writeValue(i, 1, inputs.head, ls, instruction), inputs.tail)
      case 4 =>
        outputs += readValues(1, i, ls, instruction).head
        ProgramState(i + 2, ls, inputs, isFinished = false)
      case 5 => iterateProgramRec(jump(i, ls, isJumpIfTrue = true, instruction).toInt, ls, inputs)
      case 6 => iterateProgramRec(jump(i, ls, isJumpIfTrue = false, instruction).toInt, ls, inputs)
      case 7 => checkEquality(i, ls, lt, instruction, inputs)
      case 8 => checkEquality(i, ls, eq, instruction, inputs)
      case 9 =>
        relativeBase += readValues(1, i, ls, instruction).head
        iterateProgramRec(i + 2, ls, inputs)
      case 99 => ProgramState(i, ls, inputs, isFinished = true)
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

  private def operate(startIndex: Int, ls: Map[Int, Long], fn: (Long, Long) => Long,
                      instruction: Instruction): Map[Int, Long] = {
    val values = readValues(2, startIndex, ls, instruction)
    writeValue(startIndex, 3, fn(values(0), values(1)), ls, instruction)
  }

  private def jump(startIndex: Int, ls: Map[Int, Long], isJumpIfTrue: Boolean, instruction: Instruction): Long = {
    val values = readValues(2, startIndex, ls, instruction)
    val comparison = if (isJumpIfTrue) values(0) != 0 else values(0) == 0
    if (comparison) values(1) else startIndex + 3
  }

  private def checkEquality(startIndex: Int, ls: Map[Int, Long], fn: (Long, Long) => Boolean, instruction: Instruction,
                            inputs: List[Long]): ProgramState = {
    val values = readValues(2, startIndex, ls, instruction)
    val comparison = if (fn(values(0), values(1))) 1 else 0
    val value = writeValue(startIndex, 3, comparison, ls, instruction)
    iterateProgramRec(startIndex + 4, value, inputs)
  }

  private def readValues(numValues: Int, startIndex: Int, ls: Map[Int, Long],
                         instruction: Instruction): IndexedSeq[Long] = {
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

  private def writeValue(startIndex: Int, subIndex: Int, value: Long, ls: Map[Int, Long],
                         instruction: Instruction): Map[Int, Long] = {
    if (instruction.relativeModes.contains(subIndex - 1))
      ls.updated(ls(startIndex + subIndex).toInt + relativeBase.toInt, value)
    else
      ls.updated(ls(startIndex + subIndex).toInt, value)
  }

  private def convertInputToMap(ls: List[Long]): Map[Int, Long] =
    ls.zipWithIndex.map { case (value, i) => (i, value) }.toMap.withDefaultValue(0L)

  private val add = (a: Long, b: Long) => a + b
  private val mult = (a: Long, b: Long) => a * b
  private val lt = (a: Long, b: Long) => a < b
  private val eq = (a: Long, b: Long) => a == b
}
