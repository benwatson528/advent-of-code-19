package uk.co.hadoopathome.adventofcode19

package object day12 {

  case class Coord(x: Int, y: Int, z: Int)

  case class Moon(position: Coord, velocity: Coord)

  sealed abstract class Axis(val name: String)

  case object X extends Axis("x")

  case object Y extends Axis("x")

  case object Z extends Axis("x")

}
