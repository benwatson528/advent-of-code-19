package uk.co.hadoopathome.adventofcode19.day05

object SunnyWithAChanceOfAsteroids {

  private case class Instruction(opcode: Int, modes: Set[Int])

  def runProgram(ls: IndexedSeq[Int], inputNumbers: List[Int]): Int = {
    iterateProgramRec(0, ls, List[Int](), inputNumbers).last
  }

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, ls: IndexedSeq[Int], outputs: List[Int], inputNumbers: List[Int]): List[Int] = {
    val instructions = parseInstruction(ls(i))
    instructions.opcode match {
      case 1 => iterateProgramRec(i + 4, operate(i, ls, add, instructions.modes), outputs, inputNumbers)
      case 2 => iterateProgramRec(i + 4, operate(i, ls, mult, instructions.modes), outputs, inputNumbers)
      case 3 => iterateProgramRec(i + 2, ls.updated(ls(i + 1), inputNumbers.head), outputs, inputNumbers.tail)
      case 4 => iterateProgramRec(i + 2, ls, outputs :+ getValues(ls, 1, i, instructions.modes).head, inputNumbers)
      case 5 => iterateProgramRec(jump(i, ls, isJumpIfTrue = true, instructions.modes), ls, outputs, inputNumbers)
      case 6 => iterateProgramRec(jump(i, ls, isJumpIfTrue = false, instructions.modes), ls, outputs, inputNumbers)
      case 7 => checkEquality(i, ls, lt, instructions.modes, outputs, inputNumbers)
      case 8 => checkEquality(i, ls, eq, instructions.modes, outputs, inputNumbers)
      case 99 => outputs
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
                            outputs: List[Int], inputNumbers: List[Int]): List[Int] = {
    val values = getValues(ls, 2, startIndex, modes)
    val comparison = if (fn(values(0), values(1))) 1 else 0
    iterateProgramRec(startIndex + 4, ls.updated(ls(startIndex + 3), comparison), outputs, inputNumbers)
  }

  private def getValues(ls: IndexedSeq[Int], numValues: Int, startIndex: Int, modes: Set[Int]): IndexedSeq[Int] =
    for (i <- 0 until numValues) yield if (modes.contains(i)) ls(startIndex + i + 1) else ls(ls(startIndex + i + 1))

  private val add = (a: Int, b: Int) => a + b
  private val mult = (a: Int, b: Int) => a * b
  private val lt = (a: Int, b: Int) => a < b
  private val eq = (a: Int, b: Int) => a == b
}
