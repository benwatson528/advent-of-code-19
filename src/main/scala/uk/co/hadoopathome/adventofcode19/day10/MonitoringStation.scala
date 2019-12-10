package uk.co.hadoopathome.adventofcode19.day10

import scala.collection.immutable.SortedMap

object MonitoringStation {

  def findSafestAsteroid(grid: Grid): (Coord, Int) =
    getAsteroids(grid)
        .map(baseAsteroid => (baseAsteroid, getAsteroidsInView(baseAsteroid, getAsteroids(grid, baseAsteroid))))
        .maxBy(_._2)

  def vaporiseAsteroids(grid: Grid, base: Coord, desiredAsteroid: Int): Coord = {
    val asteroidsByAngle = getAsteroids(grid, base).map(a => (angleBetweenPoints(a, base), a)).sortBy(_._1)
    val groupedByAngle = SortedMap(asteroidsByAngle.groupBy(_._1).toSeq: _*).values
    val sortedByDistance = groupedByAngle.map(_.sortBy(p => distanceBetweenPoints(base, p._2)))
    removeElementsRec(sortedByDistance.toList, 0, desiredAsteroid)._2
  }

  @scala.annotation.tailrec
  private def removeElementsRec(ls: List[List[(Double, Coord)]], numRemoved: Int, desiredAsteroid: Int): (Double, Coord) =
    numRemoved match {
      case x if x + ls.size < desiredAsteroid => removeElementsRec(ls.map(_.tail), numRemoved + ls.size, desiredAsteroid)
      case _ => ls(desiredAsteroid - numRemoved - 1).head
    }

  private def getAsteroidsInView(baseAsteroid: Coord, allAsteroids: List[Coord]): Int =
    allAsteroids.map(a => (a, angleBetweenPoints(a, baseAsteroid))).groupBy(c => c._2).keys.size

  private def angleBetweenPoints(p2: Coord, p1: Coord): Double = {
    val angle = math.toDegrees(math.atan2(p2.y - p1.y, p2.x - p1.x)) + 90
    if (angle < 0) angle + 360 else angle
  }

  private def distanceBetweenPoints(p1: Coord, p2: Coord): Double =
    math.sqrt(math.pow(p2.x - p1.x, 2) + math.pow(p2.y - p1.y, 2))

  private def getAsteroids(grid: Grid, asteroidToFilter: Coord): List[Coord] =
    grid.filter(p => p._2 == Asteroid && p._1 != asteroidToFilter).keys.toList

  private def getAsteroids(grid: Grid): List[Coord] = grid.filter(_._2 == Asteroid).keys.toList
}
