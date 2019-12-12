package uk.co.hadoopathome.adventofcode19.day12

object TheNBodyProblem {

  def calculateTotalEnergy(moons: List[Moon], steps: Int): Int = {
    val movedMoons = moveMoonsRec(moons, 0, steps)
    movedMoons.map(calculateTotalEnergy).sum
  }

  def findRepeatedStartState(moons: List[Moon]): Long =
    lcm(List(X, Y, Z).map(moveMoonsFindPatternRec(moons, 0L, moons, _)))

  @scala.annotation.tailrec
  private def moveMoonsRec(moons: List[Moon], currentStep: Int, totalSteps: Int): List[Moon] = currentStep match {
    case i if i < totalSteps =>
      moveMoonsRec(moons.map(m => applyVelocity(applyGravity(m, moons, None), None)), currentStep + 1, totalSteps)
    case _ => moons
  }

  @scala.annotation.tailrec
  private def moveMoonsFindPatternRec(moons: List[Moon], currentStep: Long, originalMoons: List[Moon],
                                      axis: Axis): Long = currentStep match {
    case _ if (currentStep != 0 && haveSameValue(moons, originalMoons, axis)) => currentStep
    case _ => moveMoonsFindPatternRec(moons.map(m => applyVelocity(applyGravity(m, moons, Some(axis)), Some(axis))),
      currentStep + 1, originalMoons, axis)
  }

  private def applyGravity(moon: Moon, moons: List[Moon], axes: Option[Axis]): Moon = {
    val gravityChanges = moons.map(applyGravity(moon, _, axes))
    val newVelocity = gravityChanges.foldLeft(moon.velocity)((a, b) => Coord(a.x + b.x, a.y + b.y, a.z + b.z))
    moon.copy(velocity = newVelocity)
  }

  private def applyGravity(moon1: Moon, moon2: Moon, axes: Option[Axis]): Coord = {
    val m1p = moon1.position
    val m2p = moon2.position
    axes match {
      case Some(a) => applyGravitySingleAxis(m1p, m2p, a)
      case None => Coord(compareGravity(m1p.x, m2p.x), compareGravity(m1p.y, m2p.y), compareGravity(m1p.z, m2p.z))
    }
  }

  def applyGravitySingleAxis(m1p: Coord, m2p: Coord, axis: Axis): Coord = axis match {
    case X => Coord(compareGravity(m1p.x, m2p.x), 0, 0)
    case Y => Coord(0, compareGravity(m1p.y, m2p.y), 0)
    case Z => Coord(0, 0, compareGravity(m1p.z, m2p.z))
  }

  private def compareGravity(a: Int, b: Int): Int = if (a < b) 1 else if (a > b) -1 else 0

  private def applyVelocity(moon: Moon, axes: Option[Axis]): Moon = {
    val p = moon.position
    val v = moon.velocity
    axes match {
      case Some(a) => moon.copy(position = applyVelocitySingleAxis(p, v, a))
      case None => moon.copy(position = Coord(p.x + v.x, p.y + v.y, p.z + v.z))
    }
  }

  private def applyVelocitySingleAxis(p: Coord, v: Coord, axis: Axis): Coord = axis match {
    case X => Coord(p.x + v.x, 0, 0)
    case Y => Coord(0, p.y + v.y, 0)
    case Z => Coord(0, 0, p.z + v.z)
  }

  private def calculateTotalEnergy(moon: Moon): Int = {
    val c = moon.position
    val potentialEnergy = c.x.abs + c.y.abs + c.z.abs
    val v = moon.velocity
    val kineticEnergy = v.x.abs + v.y.abs + v.z.abs
    potentialEnergy * kineticEnergy
  }

  private def haveSameValue(moons: List[Moon], originalMoons: List[Moon], axis: Axis): Boolean =
    moons.zip(originalMoons).forall(
      p => getPosition(p._1, axis) == getPosition(p._2, axis) && getVelocity(p._1, axis) == 0)

  private def getVelocity(moon: Moon, axis: Axis): Int = getCoord(moon.velocity, axis)

  private def getPosition(moon: Moon, axis: Axis): Int = getCoord(moon.position, axis)

  private def getCoord(coord: Coord, axis: Axis): Int = axis match {
    case X => coord.x
    case Y => coord.y
    case Z => coord.z
  }

  //Taken from https://stackoverflow.com/a/40596079/729819
  @scala.annotation.tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  private def lcm(a: Long, b: Long): Long = if (a == 0 || b == 0) 0 else a * b / gcd(a, b)

  private def lcm(nums: Iterable[Long]): Long = nums.reduce(lcm)
}
