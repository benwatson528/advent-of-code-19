package uk.co.hadoopathome.adventofcode19.day06

import scala.collection.mutable

object UniversalOrbitMap {

  type Orbit = (String, String)
  val knownDistances: mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()

  def countOrbits(ls: List[String]): Int = {
    val (orbits, planets) = parseInput(ls)
    planets.map(p => calculateDistanceToCentreRec(p, p, orbits, 0)).sum
  }

  def minimumTravel(ls: List[String], startPointer: String, endPointer: String): Int = {
    val (orbits, _) = parseInput(ls)
    val (startPlanet, endPlanet) = (getChild(startPointer, orbits), getChild(endPointer, orbits))
    jumpToDestinationRec(startPlanet, Set[String](), orbits, endPlanet, 0)
  }

  private def getLinkedPlanets(planet: String, orbits: List[Orbit]): List[String] =
    orbits.filter(o => o._1 == planet || o._2 == planet).map(o => if (o._1 == planet) o._2 else o._1)

  @scala.annotation.tailrec
  private def calculateDistanceToCentreRec(startPlanet: String, currentPlanet: String, orbits: List[Orbit],
                                           steps: Int): Int = currentPlanet match {
    case x if (x == "COM") =>
      knownDistances += (startPlanet -> steps)
      steps
    case _ if (knownDistances.contains(currentPlanet)) =>
      val distanceFromCurrent = knownDistances(currentPlanet)
      knownDistances += (startPlanet -> (steps + distanceFromCurrent))
      distanceFromCurrent + steps
    case x => calculateDistanceToCentreRec(startPlanet, getChild(x, orbits), orbits, steps + 1)
  }

  private def jumpToDestinationRec(currentPlanet: String, travelled: Set[String], orbits: List[Orbit],
                                   endPlanet: String, steps: Int): Int = currentPlanet match {
    case x if (x == endPlanet) => steps
    case x if (travelled.contains(x)) => Integer.MAX_VALUE
    case x =>
      val linkedPlanets = getLinkedPlanets(x, orbits)
      val planets = for (p <- linkedPlanets) yield jumpToDestinationRec(p, travelled + x, orbits, endPlanet, steps + 1)
      planets.min
  }

  private def getChild(planet: String, orbits: List[Orbit]): String = orbits.find(_._2 == planet).get._1

  private def parseInput(ls: List[String]): (List[Orbit], List[String]) = {
    val orbits = ls.map(x => (x.split(')')(0), x.split(')')(1)))
    val planets = orbits.flatMap(o => Set(o._1, o._2)).distinct
    (orbits, planets)
  }
}
