package uk.co.hadoopathome.adventofcode19

package object day12 {

  case class Coord(x: Int, y: Int, z: Int)

  case class Moon(position: Coord, velocity: Coord)

  sealed abstract class Axis

  case object X extends Axis

  case object Y extends Axis

  case object Z extends Axis

}
