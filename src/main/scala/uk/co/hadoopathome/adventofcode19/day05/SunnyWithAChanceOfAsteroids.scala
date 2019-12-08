package uk.co.hadoopathome.adventofcode19.day05

object SunnyWithAChanceOfAsteroids {

  case class ProgramState(pointer: Int, ls: IndexedSeq[Int], inputs: List[Int], output: Option[Int], isFinished: Boolean)

  private case class Instruction(opcode: Int, modes: Set[Int])

  def runProgram(ls: IndexedSeq[Int], inputNumbers: List[Int]): Int =
    iterateProgramRec(0, ls, List[Int](), inputNumbers).last

  def runProgramPause(programState: ProgramState): ProgramState =
    iterateProgramPauseRec(programState.pointer, programState.ls, programState.inputs)

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, ls: IndexedSeq[Int], outputs: List[Int], inputs: List[Int]): List[Int] = {
    val instructions = parseInstruction(ls(i))
    instructions.opcode match {
      case 1 => iterateProgramRec(i + 4, operate(i, ls, add, instructions.modes), outputs, inputs)
      case 2 => iterateProgramRec(i + 4, operate(i, ls, mult, instructions.modes), outputs, inputs)
      case 3 => iterateProgramRec(i + 2, ls.updated(ls(i + 1), inputs.head), outputs, inputs.tail)
      case 4 => iterateProgramRec(i + 2, ls, outputs :+ getValues(ls, 1, i, instructions.modes).head, inputs)
      case 5 => iterateProgramRec(jump(i, ls, isJumpIfTrue = true, instructions.modes), ls, outputs, inputs)
      case 6 => iterateProgramRec(jump(i, ls, isJumpIfTrue = false, instructions.modes), ls, outputs, inputs)
      case 7 => checkEquality(i, ls, lt, instructions.modes, outputs, inputs)
      case 8 => checkEquality(i, ls, eq, instructions.modes, outputs, inputs)
      case 99 => outputs
    }
  }

  @scala.annotation.tailrec
  private def iterateProgramPauseRec(i: Int, ls: IndexedSeq[Int], inputs: List[Int]): ProgramState = {
    val instructions = parseInstruction(ls(i))
    instructions.opcode match {
      case 1 => iterateProgramPauseRec(i + 4, operate(i, ls, add, instructions.modes), inputs)
      case 2 => iterateProgramPauseRec(i + 4, operate(i, ls, mult, instructions.modes), inputs)
      case 3 => iterateProgramPauseRec(i + 2, ls.updated(ls(i + 1), inputs.head), inputs.tail)
      case 4 => ProgramState(i + 2, ls, inputs, Some(getValues(ls, 1, i, instructions.modes).head), isFinished = false)
      case 5 => iterateProgramPauseRec(jump(i, ls, isJumpIfTrue = true, instructions.modes), ls, inputs)
      case 6 => iterateProgramPauseRec(jump(i, ls, isJumpIfTrue = false, instructions.modes), ls, inputs)
      case 7 => checkEqualityPause(i, ls, lt, instructions.modes, inputs)
      case 8 => checkEqualityPause(i, ls, eq, instructions.modes, inputs)
      case 99 => ProgramState(0, ls, List[Int](), None, isFinished = true)
    }
  }

  private def parseInstruction(i: Int): Instruction = {
    val rawInstruction = i.toString
    val values = for (i <- rawInstruction.length - 2 to 0 by -1; if (rawInstruction(i) == '1'))
      yield rawInstruction.length - 3 - i
    Instruction(i % 100, values.toSet)
  }

  private def operate(startIndex: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Int, modes: Set[Int]): IndexedSeq[Int] = {
    val values = getValues(ls, 2, startIndex, modes)
    ls.updated(ls(startIndex + 3), fn(values(0), values(1)))
  }

  private def jump(startIndex: Int, ls: IndexedSeq[Int], isJumpIfTrue: Boolean, modes: Set[Int]): Int = {
    val values = getValues(ls, 2, startIndex, modes)
    val comparison = if (isJumpIfTrue) values(0) != 0 else values(0) == 0
    if (comparison) values(1) else startIndex + 3
  }

  private def checkEquality(startIndex: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Boolean, modes: Set[Int],
                            outputs: List[Int], inputs: List[Int]): List[Int] = {
    val values = getValues(ls, 2, startIndex, modes)
    val comparison = if (fn(values(0), values(1))) 1 else 0
    iterateProgramRec(startIndex + 4, ls.updated(ls(startIndex + 3), comparison), outputs, inputs)
  }

  private def checkEqualityPause(startIndex: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Boolean, modes: Set[Int],
                                 inputs: List[Int]): ProgramState = {
    val values = getValues(ls, 2, startIndex, modes)
    val comparison = if (fn(values(0), values(1))) 1 else 0
    iterateProgramPauseRec(startIndex + 4, ls.updated(ls(startIndex + 3), comparison), inputs)
  }

  private def getValues(ls: IndexedSeq[Int], numValues: Int, startIndex: Int, modes: Set[Int]): IndexedSeq[Int] =
    for (i <- 0 until numValues) yield if (modes.contains(i)) ls(startIndex + i + 1) else ls(ls(startIndex + i + 1))

  private val add = (a: Int, b: Int) => a + b
  private val mult = (a: Int, b: Int) => a * b
  private val lt = (a: Int, b: Int) => a < b
  private val eq = (a: Int, b: Int) => a == b
}
