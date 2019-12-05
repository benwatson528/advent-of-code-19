package uk.co.hadoopathome.adventofcode19.day05

object SunnyWithAChanceOfAsteroids {
  var INPUT_NUMBER = 0

  case class Instruction(opcode: Int, modes: Set[Int])

  def runProgram(ls: IndexedSeq[Int], inputNumber: Int): List[Int] = {
    INPUT_NUMBER = inputNumber
    iterateProgramRec(0, ls, List[Int]())
  }

  def parseInstruction(i: Int): Instruction = {
    if (i==99) Instruction(99, Set[Int]())
    else if (i < 9) Instruction(i, Set[Int]())
    else {
      val commandStr = i.toString.reverse
      val value = commandStr.tail.tail.zipWithIndex.filter(x => x._1 == '1').map(_._2).toSet
      Instruction(commandStr.reverse.toInt % 100, value)
    }
  }

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, ls: IndexedSeq[Int], outputs: List[Int]): List[Int] = {
    val instructions = parseInstruction(ls(i))
    instructions.opcode match {
      case 99 => outputs
      case 1 => iterateProgramRec(i + 4, operate(i, ls, +, instructions.modes), outputs)
      case 2 => iterateProgramRec(i + 4, operate(i, ls, *, instructions.modes), outputs)
      case 3 => iterateProgramRec(i + 2, ls.updated(ls(i + 1), INPUT_NUMBER), outputs)
      case 4 => iterateProgramRec(i + 2, ls, outputs :+ getValue(ls, i + 1, instructions.modes.contains(0)))
      case 5 =>
        val firstArg = getValue(ls, i + 1, instructions.modes.contains(0))
        val secondArg = getValue(ls, i + 2, instructions.modes.contains(1))
        if (firstArg != 0) iterateProgramRec(secondArg, ls, outputs) else iterateProgramRec(i + 3, ls, outputs)
      case 6 =>
        val firstArg = getValue(ls, i + 1, instructions.modes.contains(0))
        val secondArg = getValue(ls, i + 2, instructions.modes.contains(1))
        if (firstArg == 0) iterateProgramRec(secondArg, ls, outputs) else iterateProgramRec(i + 3, ls, outputs)
      case 7 =>
        val firstArg = getValue(ls, i + 1, instructions.modes.contains(0))
        val secondArg = getValue(ls, i + 2, instructions.modes.contains(1))
        val comparison = if (firstArg < secondArg) 1 else 0
        iterateProgramRec(i + 4, ls.updated(ls(i + 3), comparison), outputs)
      case 8 =>
        val firstArg = getValue(ls, i + 1, instructions.modes.contains(0))
        val secondArg = getValue(ls, i + 2, instructions.modes.contains(1))
        val comparison = if (firstArg == secondArg) 1 else 0
        iterateProgramRec(i + 4, ls.updated(ls(i + 3), comparison), outputs)
    }
  }

  private def operate(startIndex: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Int, modes: Set[Int]): IndexedSeq[Int] = {
    val firstArg = getValue(ls, startIndex + 1, modes.contains(0))
    val secondArg = getValue(ls, startIndex + 2, modes.contains(1))
    ls.updated(ls(startIndex + 3), fn(firstArg, secondArg))
  }

  private def getValue(ls: IndexedSeq[Int], index: Int, isImmediateMode: Boolean): Int =
    if (isImmediateMode) ls(index) else ls(ls(index))

  private val + = (a: Int, b: Int) => a + b
  private val * = (a: Int, b: Int) => a * b
}
