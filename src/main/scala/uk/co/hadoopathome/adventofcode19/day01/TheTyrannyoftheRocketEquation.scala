package uk.co.hadoopathome.adventofcode19.day01

object TheTyrannyoftheRocketEquation {
  def sumFuelRequirements(ls: List[Int]): Int = {
    sumFuelRequirementsRec(ls, 0)
  }

  @scala.annotation.tailrec
  private def sumFuelRequirementsRec(ls: List[Int], sum: Int): Int = {
    ls match {
      case x :: xx => sumFuelRequirementsRec(xx, sum + calculateFuelRequired(x))
      case _ => sum
    }
  }

  private def calculateFuelRequired(mass: Int): Int = (mass / 3) - 2
}
