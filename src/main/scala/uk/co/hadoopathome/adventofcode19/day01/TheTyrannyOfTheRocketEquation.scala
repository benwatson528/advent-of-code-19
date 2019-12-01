package uk.co.hadoopathome.adventofcode19.day01

object TheTyrannyOfTheRocketEquation {
  def sumFuelRequirements(ls: List[Int], includeFuelWeight: Boolean): Int = {
    sumFuelRequirementsRec(ls, 0, includeFuelWeight)
  }

  @scala.annotation.tailrec
  private def sumFuelRequirementsRec(ls: List[Int], sum: Int, includeFuelWeight: Boolean): Int = {
    ls match {
      case x :: xx => sumFuelRequirementsRec(xx, sum + calculateFuelRequired(x, includeFuelWeight), includeFuelWeight)
      case _ => sum
    }
  }

  private def calculateFuelRequired(mass: Int, includeFuelWeight: Boolean): Int = {
    if (!includeFuelWeight)
      calculateFuelForMass(mass)
    else calculateFuelRequiredRec(0, mass)
  }

  @scala.annotation.tailrec
  private def calculateFuelRequiredRec(totalMass: Int, latestFuelAmount: Int): Int = {
    val latestSum = calculateFuelForMass(latestFuelAmount)
    if (latestSum <= 0)
      totalMass
    else
      calculateFuelRequiredRec(totalMass + latestSum, latestSum)
  }

  private def calculateFuelForMass(mass: Int) = (mass / 3) - 2
}
