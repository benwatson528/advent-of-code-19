package uk.co.hadoopathome.adventofcode19.day13

import uk.co.hadoopathome.adventofcode19.day05.Intcode

object CarePackage {

  def populateTiles(ls: List[Long]): List[Tile] = {
    val intcode = new Intcode(ls)
    readCommandsRec(intcode, List[Command](), None).filter(_.isInstanceOf[Tile]).asInstanceOf[List[Tile]]
  }

  def playGame(ls: List[Long]): List[Command] = {
    val gameProgram = ls.updated(0, 2L)
    val intcode = new Intcode(gameProgram)
    readCommandsRec(intcode, List[Command](), None)
  }

  @scala.annotation.tailrec
  private def readCommandsRec(intcode: Intcode, tiles: List[Command],
                              joystickMovement: Option[Long]): List[Command] = {
    val newCommand = readCommand(intcode, joystickMovement)
    newCommand match {
      case Some(Score(_, _, score)) =>
        //Do something here based on the score?
        readCommandsRec(intcode, tiles, Some(-1))
      case Some(Tile(x, y, id)) => readCommandsRec(intcode, tiles :+ Tile(x, y, id), None)
      case None => tiles
    }
  }

  private def readCommand(intcode: Intcode, joystickMovement: Option[Long]): Option[Command] = {
    val (x, isFinished) = intcode.runUntilPause(joystickMovement)
    if (isFinished) return None
    val (y, _) = intcode.runUntilPause(None)
    val (param, _) = intcode.runUntilPause(None)
    if (x == -1 && y == 0) Some(Score(x.toInt, y.toInt, param)) else Some(Tile(x.toInt, y.toInt, mapToId(param)))
  }

  private def mapToId(id: Long): Id = id match {
    case 0 => EMPTY
    case 1 => WALL
    case 2 => BLOCK
    case 3 => HORIZONTAL
    case 4 => BALL
  }
}
