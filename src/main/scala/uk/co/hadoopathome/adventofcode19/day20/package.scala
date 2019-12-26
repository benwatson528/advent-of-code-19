package uk.co.hadoopathome.adventofcode19

package object day20 {

  case class Coord(x: Int, y: Int)

  type Maze = Map[Coord, Item]
  
  type Portals = Map[Coord, Option[Coord]]

  sealed abstract class Item

  case object PASSAGE extends Item

  case object WALL extends Item

  case object EMPTY extends Item

  case class PORTAL(portalId: String) extends Item
}
