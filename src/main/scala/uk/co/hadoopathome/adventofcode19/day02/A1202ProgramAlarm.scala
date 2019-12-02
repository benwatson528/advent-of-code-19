package uk.co.hadoopathome.adventofcode19.day02


object A1202ProgramAlarm {
  def runProgram(ls: IndexedSeq[Int]): IndexedSeq[Int] = iterateProgramRec(0, ls)

  @scala.annotation.tailrec
  private def iterateProgramRec(i: Int, ls: IndexedSeq[Int]): IndexedSeq[Int] = {
    if (ls(i) == 1) iterateProgramRec(i + 4, operate(i, ls, +))
    else if (ls(i) == 2) iterateProgramRec(i + 4, operate(i, ls, *))
    else ls
  }

  private def operate(i: Int, ls: IndexedSeq[Int], fn: (Int, Int) => Int): IndexedSeq[Int] =
    ls.updated(ls(i + 3), fn(ls(ls(i + 1)), ls(ls(i + 2))))

  private val + = (a: Int, b: Int) => a + b
  private val * = (a: Int, b: Int) => a * b
}
