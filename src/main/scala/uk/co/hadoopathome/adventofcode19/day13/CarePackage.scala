package uk.co.hadoopathome.adventofcode19.day13

import uk.co.hadoopathome.adventofcode19.day05.Intcode
import uk.co.hadoopathome.adventofcode19.day11.SpacePolice.Coord

object CarePackage {

  def populateBoard(ls: List[Long]): Long = readCommandsRec(new Intcode(ls), None, None, Set[Coord]())

  def playGame(ls: List[Long]): Long = readCommandsRec(new Intcode(ls.updated(0, 2L)), None, None, Set[Coord]())

  @scala.annotation.tailrec
  private def readCommandsRec(intcode: Intcode, paddle: Option[Coord], ball: Option[Coord],
                              blocks: Set[Coord]): Long = {
    val newCommand = readCommand(intcode)
    newCommand match {
      case Some(Score(_, _, score)) =>
        if (blocks.isEmpty) score else readCommandsRec(intcode, paddle, ball, blocks)
      case Some(Tile(x, y, id)) => id match {
        case PADDLE =>
          intcode.provideInput(moveJoystick(Some(Coord(x, y)), ball))
          readCommandsRec(intcode, Some(Coord(x, y)), ball, blocks)
        case BALL =>
          intcode.provideInput(moveJoystick(paddle, Some(Coord(x, y))))
          readCommandsRec(intcode, paddle, Some(Coord(x, y)), blocks)
        case BLOCK =>
          readCommandsRec(intcode, paddle, ball, blocks + Coord(x, y))
        case WALL =>
          readCommandsRec(intcode, paddle, ball, blocks)
        case EMPTY =>
          if (blocks.contains(Coord(x, y))) readCommandsRec(intcode, paddle, ball, blocks - Coord(x, y))
          else readCommandsRec(intcode, paddle, ball, blocks)
      }
      case None => blocks.size
    }
  }

  private def moveJoystick(paddle: Option[Coord], ball: Option[Coord]): Long = {
    if (paddle.isEmpty || ball.isEmpty) 0
    else {
      val paddleX = paddle.get.x
      val ballX = ball.get.x
      if (ballX < paddleX) -1
      else if (ballX == paddleX) 0
      else 1
    }
  }

  private def readCommand(intcode: Intcode): Option[Command] = {
    val (x, isFinished) = intcode.runUntilPause(None)
    if (isFinished) return None
    val (y, _) = intcode.runUntilPause(None)
    val (param, _) = intcode.runUntilPause(None)
    if (x == -1 && y == 0) Some(Score(x.toInt, y.toInt, param)) else Some(Tile(x.toInt, y.toInt, mapToId(param)))
  }

  private def mapToId(id: Long): Id = id match {
    case 0 => EMPTY
    case 1 => WALL
    case 2 => BLOCK
    case 3 => PADDLE
    case 4 => BALL
  }
}
