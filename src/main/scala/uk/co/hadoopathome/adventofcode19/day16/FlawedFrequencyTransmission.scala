package uk.co.hadoopathome.adventofcode19.day16

object FlawedFrequencyTransmission {

  val basePattern = List(0, 1, 0, -1)

  def calculateOutput(ls: IndexedSeq[Int], totalSteps: Int): String = calculateOutputRec(ls, 0, totalSteps)

  @scala.annotation.tailrec
  private def calculateOutputRec(ls: IndexedSeq[Int], currentStep: Int, totalSteps: Int): String = {
    if (currentStep == totalSteps) ls.mkString
    else {
      val inputLength = ls.length
      var multiplier = 1
      val result = for (_ <- 0 until inputLength) yield {
        val expandedBasePattern = LazyList.continually(
          basePattern.flatMap(List.fill(multiplier)(_))).flatten.take(inputLength + 1).toList.tail
        val components = ls.zip(expandedBasePattern)
        multiplier += 1
        components.map(p => p._1 * p._2).sum.toString.last.asDigit
      }
      calculateOutputRec(result, currentStep + 1, totalSteps)
    }
  }
}
