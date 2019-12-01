package uk.co.hadoopathome.adventofcode19.day01

object TheTyrannyOfTheRocketEquation {
  def sumFuelRequirements(ls: List[Int], includeFuelWeight: Boolean): Int = {
    ls.foldLeft(0)((a, b) => a + calculateFuelRequired(b, includeFuelWeight))
  }

  private def calculateFuelRequired(mass: Int, includeFuelWeight: Boolean): Int = {
    if (includeFuelWeight)
      calculateFuelRequiredRec(0, mass)
    else
      calculateFuelForMass(mass)
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
