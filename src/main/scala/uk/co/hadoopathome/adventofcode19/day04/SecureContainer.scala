package uk.co.hadoopathome.adventofcode19.day04

import scala.language.postfixOps

object SecureContainer {

  def findNumValidPasswordsTwoAdjacent(range: Range): Int = findNumValidPasswords(range, hasAtLeastTwoAdjacentDigits)

  def findNumValidPasswordsMultipleAdjacent(range: Range): Int =
    findNumValidPasswords(range, hasExactlyTwoAdjacentDigits)

  private def findNumValidPasswords(range: Range, rule: (List[Int] => Boolean)): Int = {
    val startList = if (isAscending(range.head)) List[Int](range.head) else List[Int]()
    val ascendingNumbers = getAscendingNumbersRec(range.head, range.last, startList)
    ascendingNumbers.count(x => rule(intToList(x)))
  }

  @scala.annotation.tailrec
  private def getAscendingNumbersRec(start: Int, end: Int, ascendingNumbers: List[Int]): List[Int] = {
    val nextNumber = getNextAscendingNumber(start + 1)
    if (nextNumber > end) return ascendingNumbers
    getAscendingNumbersRec(nextNumber, end, ascendingNumbers :+ nextNumber)
  }

  private def getNextAscendingNumber(x: Int): Int = {
    val n = intToList(x)
    val rootSize = n.sliding(2).map(x => (x.head, x.tail.head)).takeWhile(p => p._1 <= p._2).length + 1
    val newRoot = n.take(rootSize)
    (newRoot ::: List.fill(6 - rootSize)(newRoot.last)).mkString.toInt
  }

  private def isAscending(n: Int): Boolean =
    !intToList(n).sliding(2).map(x => (x.head, x.tail.head)).exists(p => p._1 > p._2)

  private def hasAtLeastTwoAdjacentDigits(password: List[Int]): Boolean =
    password.sliding(2).map(x => (x.head, x.tail.head)).exists(p => p._1 == p._2)

  private def hasExactlyTwoAdjacentDigits(password: List[Int]): Boolean =
    groupConsecutive(password).count(_.length == 2) != 0

  //https://stackoverflow.com/questions/4761386/scala-list-function-for-grouping-consecutive-identical-elements
  private def groupConsecutive(ls: List[Int]): List[List[Int]] = ls match {
    case Nil => Nil
    case x :: _ => val segment = ls takeWhile {x ==}
      segment :: groupConsecutive(ls drop segment.length)
  }

  private def intToList(n: Int): List[Int] = n.toString.map(_.asDigit).toList
}
