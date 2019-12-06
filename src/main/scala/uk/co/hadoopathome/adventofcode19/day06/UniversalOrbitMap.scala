package uk.co.hadoopathome.adventofcode19.day06

object UniversalOrbitMap {

  type Orbit = (String, String)

  def countOrbits(ls: List[String]): Int = {
    val orbits = ls.map(x => (x.split(')')(0), x.split(')')(1)))
    val planets = orbits.flatMap(o => Set(o._1, o._2)).distinct
    planets.map(p => calculateDistanceToCentreRec(p, orbits, 0)).sum
  }

  @scala.annotation.tailrec
  private def calculateDistanceToCentreRec(planet: String, orbits: List[Orbit], steps: Int): Int = planet match {
    case x if (x == "COM") => steps
    case x => calculateDistanceToCentreRec(getChild(x, orbits), orbits, steps + 1)
  }

  private def getChild(planet: String, orbits: List[Orbit]): String = orbits.find(_._2 == planet).get._1
}
