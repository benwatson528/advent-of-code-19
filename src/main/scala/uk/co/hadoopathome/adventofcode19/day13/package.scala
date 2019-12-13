package uk.co.hadoopathome.adventofcode19

package object day13 {


  sealed abstract class Command()

  case class Score(x: Int, y: Int, score: Long) extends Command

  case class Tile(x: Int, y: Int, id: Id) extends Command

  type Board = Vector[Tile]

  sealed abstract class Id()

  case object EMPTY extends Id

  case object WALL extends Id

  case object BLOCK extends Id

  case object PADDLE extends Id

  case object BALL extends Id

  def printBoard(tiles: List[Tile]): Unit = {
    val (minX, maxX) = (tiles.minBy(_.x).x, tiles.maxBy(_.x).x)
    val (minY, maxY) = (tiles.minBy(_.y).y, tiles.maxBy(_.y).y)

    for (y <- minY to maxY) {
      println()
      for (x <- minX to maxX) {
        val tileAtPosition = tiles.find(t => t.x == x && t.y == y)
        tileAtPosition match {
          case Some(t) if t.id == EMPTY => print(" ")
          case Some(t) if t.id == WALL => print("█")
          case Some(t) if t.id == BLOCK => print("-")
          case Some(t) if t.id == PADDLE => print("=")
          case Some(t) if t.id == BALL => print("*")
        }
      }
    }
    println()
  }
}
