package uk.co.hadoopathome.adventofcode19.day04

object SecureContainer {

  def findNumValidPasswords(range: Range): Int =
    range.count(isValidPassword)

  private def isValidPassword(password: Int): Boolean = {
    val passwordStr = password.toString
    if (!hasAdjacentDigits(passwordStr)) return false
    if (!digitsIncrease(passwordStr)) return false
    true
  }

  private def hasAdjacentDigits(passwordStr: String): Boolean =
    passwordStr.toSeq.sliding(2).map(x => (x.head, x.tail.head)).exists(p => p._1 == p._2)

  private def digitsIncrease(passwordStr: String): Boolean =
    passwordStr.toSeq.sliding(2).map(x => (x.head, x.tail.head)).forall(p => p._1 <= p._2)
}
