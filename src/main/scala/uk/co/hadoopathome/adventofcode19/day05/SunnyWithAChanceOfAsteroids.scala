package uk.co.hadoopathome.adventofcode19.day05

object SunnyWithAChanceOfAsteroids {

  case class Instruction(opcode: Int, modes: Set[Int])

  def runProgram(ls: IndexedSeq[Int], inputNumber: Int): List[Int] = {
    iterateProgramRec(0, ls, List[Int](), inputNumber)
  }

  def parseInstruction(i: Int): Instruction = {
    if (i == 99) Instruction(99, Set[Int]())
    else if (i < 9) Instruction(i, Set[Int]())
    else {
      val commandStr = i.toString.reverse
      val value = commandStr.tail.tail.zipWithIndex.filter(x => x._1 == '1').map(_._2).toSet
      Instruction(commandStr.reverse.toInt % 100, value)
    }
  }

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, ls: IndexedSeq[Int], outputs: List[Int], inputNumber: Int): List[Int] = {
    val instructions = parseInstruction(ls(i))
    instructions.opcode match {
      case 99 => outputs
      case 1 => iterateProgramRec(i + 4, operate(i, ls, add, instructions.modes), outputs, inputNumber)
      case 2 => iterateProgramRec(i + 4, operate(i, ls, mult, instructions.modes), outputs, inputNumber)
      case 3 => iterateProgramRec(i + 2, ls.updated(ls(i + 1), inputNumber), outputs, inputNumber)
      case 4 => iterateProgramRec(i + 2, ls, outputs :+ getValues(ls, 1, i, instructions.modes).head, inputNumber)
      case 5 => jump(i, ls, isJumpIfTrue = true, instructions.modes, outputs, inputNumber)
      case 6 => jump(i, ls, isJumpIfTrue = false, instructions.modes, outputs, inputNumber)
      case 7 => checkEquality(i, ls, lt, instructions.modes, outputs, inputNumber)
      case 8 => checkEquality(i, ls, eq, instructions.modes, outputs, inputNumber)
    }
  }

  private def getValues(ls: IndexedSeq[Int], numValues: Int, startIndex: Int, modes: Set[Int]): IndexedSeq[Int] =
    for (i <- 0 until numValues) yield if (modes.contains(i)) ls(startIndex + i + 1) else ls(ls(startIndex + i + 1))

  private def operate(startIndex: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Int, modes: Set[Int]): IndexedSeq[Int] = {
    val values = getValues(ls, 2, startIndex, modes)
    ls.updated(ls(startIndex + 3), fn(values(0), values(1)))
  }

  private def jump(startIndex: Int, ls: IndexedSeq[Int], isJumpIfTrue: Boolean, modes: Set[Int], outputs: List[Int],
                   inputNumber: Int): List[Int] = {
    val values = getValues(ls, 2, startIndex, modes)
    val doJump = if (isJumpIfTrue) values(0) != 0 else values(0) == 0
    if (doJump)
      iterateProgramRec(values(1), ls, outputs, inputNumber)
    else
      iterateProgramRec(startIndex + 3, ls, outputs, inputNumber)
  }

  private def checkEquality(startIndex: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Boolean, modes: Set[Int],
                            outputs: List[Int], inputNumber: Int): List[Int] = {
    val values = getValues(ls, 2, startIndex, modes)
    val comparison = if (fn(values(0), values(1))) 1 else 0
    iterateProgramRec(startIndex + 4, ls.updated(ls(startIndex + 3), comparison), outputs, inputNumber)
  }

  private val add = (a: Int, b: Int) => a + b
  private val mult = (a: Int, b: Int) => a * b
  private val lt = (a: Int, b: Int) => a < b
  private val eq = (a: Int, b: Int) => a == b
}
