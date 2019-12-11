package uk.co.hadoopathome.adventofcode19.day11

import uk.co.hadoopathome.adventofcode19.day05.Amplifier

object SpacePolice {

  case class Coord(x: Int, y: Int)

  type Robot = (Coord, Char)
  private val LEFT_TURNS = "^<v>".toList
  private val RIGHT_TURNS = LEFT_TURNS.reverse
  private val CENTRE = Coord(0, 0)

  def findNumPaintedPanels(ls: List[Long]): Int = {
    val amplifier = new Amplifier(ls)
    val robot = (CENTRE, '^')
    paintRec(robot, amplifier, Map[Coord, Int](CENTRE -> 0)).size
  }

  def drawRegistration(ls: List[Long]): Map[Coord, Int] = {
    val amplifier = new Amplifier(ls)
    val robot = (CENTRE, '^')
    paintRec(robot, amplifier, Map[Coord, Int](CENTRE -> 1))
  }

  @scala.annotation.tailrec
  private def paintRec(robot: Robot, amplifier: Amplifier, painted: Map[Coord, Int]): Map[Coord, Int] = {
    val currentColour = painted.getOrElse(robot._1, 0)
    val (newColour, isFinished) = amplifier.runWithPause(currentColour)
    val updatedPainted = if (currentColour != newColour.toInt) painted + (robot._1 -> newColour.toInt) else painted
    if (isFinished) return updatedPainted
    val turnDirection = amplifier.runWithPause()._1.toInt
    val newRobot = moveRobot(turnRobot(robot, turnDirection))
    paintRec(newRobot, amplifier, updatedPainted)
  }

  private def turnRobot(robot: Robot, turnDirection: Int): Robot = turnDirection match {
    case 0 => (robot._1, LEFT_TURNS((LEFT_TURNS.indexOf(robot._2) + 1) % 4))
    case 1 => (robot._1, RIGHT_TURNS((RIGHT_TURNS.indexOf(robot._2) + 1) % 4))
  }

  private def moveRobot(robot: Robot): Robot = robot._2 match {
    case '^' => (Coord(robot._1.x, robot._1.y + 1), robot._2)
    case '<' => (Coord(robot._1.x - 1, robot._1.y), robot._2)
    case 'v' => (Coord(robot._1.x, robot._1.y - 1), robot._2)
    case '>' => (Coord(robot._1.x + 1, robot._1.y), robot._2)
  }
}
