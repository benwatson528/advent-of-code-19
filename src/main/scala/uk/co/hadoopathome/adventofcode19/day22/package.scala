package uk.co.hadoopathome.adventofcode19

package object day22 {

  sealed abstract class Instruction

  case class CUT(n: Int) extends Instruction

  case class DEAL(n: Int = 0) extends Instruction

}
