package uk.co.hadoopathome.adventofcode19.day05

class Intcode(initialProgram: List[Long] = List[Long](), initialInputs: List[Long] = List[Long]()) {

  def this(initialProgram: List[Long], initialInput: Long) {
    this(initialProgram, List[Long](initialInput))
  }

  case class ProgramState(pointer: Int, program: Map[Int, Long], inputs: List[Long], relativeBase: Long,
                          outputs: List[Long], isFinished: Boolean = false)

  private case class Instruction(opcode: Long, modes: List[Int])

  private var pausedProgramState = ProgramState(0, convertInputToMap(initialProgram), initialInputs, 0, List[Long]())

  def runUntilHalt(): Long = runUntilHaltRec(pausedProgramState)

  def runUntilPause(input: Option[Long]): (Long, Boolean) = {
    pausedProgramState = input match {
      case Some(i) => iterateProgramRec(pausedProgramState.copy(inputs = pausedProgramState.inputs :+ i))
      case None => iterateProgramRec(pausedProgramState.copy(inputs = pausedProgramState.inputs))
    }
    (pausedProgramState.outputs.last, pausedProgramState.isFinished)
  }

  @scala.annotation.tailrec
  private def runUntilHaltRec(programState: ProgramState): Long =
    if (programState.isFinished)
      programState.outputs.last
    else
      runUntilHaltRec(iterateProgramRec(programState))

  @scala.annotation.tailrec
  private def iterateProgramRec(programState: ProgramState): ProgramState = {
    val (i, ls, inputs, relativeBase, outputs) = (programState.pointer, programState.program, programState.inputs,
        programState.relativeBase, programState.outputs)
    val instruction = parseInstruction(ls(i).toInt)
    instruction.opcode match {
      case 1 =>
        val newProgram = operate(i, ls, add, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = i + 4, program = newProgram))
      case 2 =>
        val newProgram = operate(i, ls, mult, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = i + 4, program = newProgram))
      case 3 =>
        val newProgram = writeValue(i, 1, inputs.head, ls, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = i + 2, program = newProgram, inputs = inputs.tail))
      case 4 =>
        val newOutputs = outputs :+ readValue(i, 1, ls, relativeBase, instruction)
        programState.copy(pointer = i + 2, outputs = newOutputs)
      case 5 =>
        val newPointer = jump(i, ls, isJumpIfTrue = true, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = newPointer))
      case 6 =>
        val newPointer = jump(i, ls, isJumpIfTrue = false, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = newPointer))
      case 7 =>
        val newProgram = checkEquality(i, ls, lt, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = i + 4, program = newProgram))
      case 8 =>
        val newProgram = checkEquality(i, ls, eq, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = i + 4, program = newProgram))
      case 9 =>
        val newRelativeBase = relativeBase + readValue(i, 1, ls, relativeBase, instruction)
        iterateProgramRec(programState.copy(pointer = i + 2, relativeBase = newRelativeBase))
      case 99 =>
        programState.copy(isFinished = true)
    }
  }

  private def operate(startIndex: Int, ls: Map[Int, Long], fn: (Long, Long) => Long, relativeBase: Long,
                      instruction: Instruction): Map[Int, Long] = {
    val value1 = readValue(startIndex, 1, ls, relativeBase, instruction)
    val value2 = readValue(startIndex, 2, ls, relativeBase, instruction)
    writeValue(startIndex, 3, fn(value1, value2), ls, relativeBase, instruction)
  }

  private def jump(startIndex: Int, ls: Map[Int, Long], isJumpIfTrue: Boolean, relativeBase: Long,
                   instruction: Instruction): Int = {
    val value1 = readValue(startIndex, 1, ls, relativeBase, instruction)
    val value2 = readValue(startIndex, 2, ls, relativeBase, instruction)
    val comparison = if (isJumpIfTrue) value1 != 0 else value1 == 0
    if (comparison) value2.toInt else startIndex + 3
  }

  private def checkEquality(startIndex: Int, ls: Map[Int, Long], fn: (Long, Long) => Boolean, relativeBase: Long,
                            instruction: Instruction): Map[Int, Long] = {
    val value1 = readValue(startIndex, 1, ls, relativeBase, instruction)
    val value2 = readValue(startIndex, 2, ls, relativeBase, instruction)
    val comparison = if (fn(value1, value2)) 1 else 0
    writeValue(startIndex, 3, comparison, ls, relativeBase, instruction)
  }

  private def readValue(startIndex: Int, subIndex: Int, ls: Map[Int, Long], relativeBase: Long,
                        instruction: Instruction): Long = {
    val rootIndex = startIndex + subIndex
    instruction.modes(subIndex - 1) match {
      case 0 => ls(ls(rootIndex).toInt)
      case 1 => ls(rootIndex)
      case 2 => ls(ls(rootIndex.toInt).toInt + relativeBase.toInt)
    }
  }

  private def writeValue(startIndex: Int, subIndex: Int, value: Long, ls: Map[Int, Long], relativeBase: Long,
                         instruction: Instruction): Map[Int, Long] = {
    val rootIndex = startIndex + subIndex
    instruction.modes(subIndex - 1) match {
      case 0 => ls.updated(ls(rootIndex).toInt, value)
      case 1 => throw new IllegalArgumentException("Can't write with immediate mode")
      case 2 => ls.updated(ls(rootIndex).toInt + relativeBase.toInt, value)
    }
  }

  private def parseInstruction(i: Int): Instruction = {
    val mode1 = i / 100 % 10
    val mode2 = i / 1000 % 10
    val mode3 = i / 10000 % 10
    Instruction(i % 100, List(mode1, mode2, mode3))
  }

  private def convertInputToMap(ls: List[Long]): Map[Int, Long] =
    ls.zipWithIndex.map { case (value, i) => (i, value) }.toMap.withDefaultValue(0L)

  private val add = (a: Long, b: Long) => a + b
  private val mult = (a: Long, b: Long) => a * b
  private val lt = (a: Long, b: Long) => a < b
  private val eq = (a: Long, b: Long) => a == b
}
