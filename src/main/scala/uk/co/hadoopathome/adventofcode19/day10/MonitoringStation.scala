package uk.co.hadoopathome.adventofcode19.day10

object MonitoringStation {

  def findSafestAsteroid(grid: Grid): (Coord, Int) = {
    getAsteroids(grid)
        .map(baseAsteroid => (baseAsteroid, getAsteroidsInView(baseAsteroid, getAsteroids(grid, baseAsteroid))))
        .maxBy(_._2)
  }

  def getAsteroidsInView(baseAsteroid: Coord, allAsteroids: List[Coord]): Int = {
    val closest = allAsteroids.map(a => (a, angleBetweenPoints(a, baseAsteroid)))
    closest.groupBy(c => c._2).keys.size
  }

  private def angleBetweenPoints(p1: Coord, p2: Coord): Double = math.atan2(p2.y - p1.y, p2.x - p1.x)

  private def getAsteroids(grid: Grid, asteroidToFilter: Coord): List[Coord] =
    grid.filter(p => p._2 == Asteroid && p._1 != asteroidToFilter).keys.toList

  private def getAsteroids(grid: Grid): List[Coord] = grid.filter(_._2 == Asteroid).keys.toList
}
