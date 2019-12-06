package uk.co.hadoopathome.adventofcode19.day06

import scala.collection.mutable

object UniversalOrbitMap {

  type Orbit = (String, String)
  val knownDistances: mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()

  def countOrbits(ls: List[String]): Int = {
    val orbits = ls.map(x => (x.split(')')(0), x.split(')')(1)))
    val planets = orbits.flatMap(o => Set(o._1, o._2)).distinct
    planets.map(p => calculateDistanceToCentreRec(p, p, orbits, 0)).sum
  }

  @scala.annotation.tailrec
  private def calculateDistanceToCentreRec(startPlanet: String, currentPlanet: String, orbits: List[Orbit], steps: Int):
  Int = currentPlanet match {
    case x if (x == "COM") =>
      knownDistances += (startPlanet -> steps)
      steps
    case _ if (knownDistances.contains(currentPlanet)) => {
      val distanceFromCurrent = knownDistances(currentPlanet)
      knownDistances += (startPlanet -> (steps + distanceFromCurrent))
      distanceFromCurrent + steps
    }
    case x => calculateDistanceToCentreRec(startPlanet, getChild(x, orbits), orbits, steps + 1)
  }

  private def getChild(planet: String, orbits: List[Orbit]): String = orbits.find(_._2 == planet).get._1
}
