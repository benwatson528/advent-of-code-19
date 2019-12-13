package uk.co.hadoopathome.adventofcode19.day13

import uk.co.hadoopathome.adventofcode19.day05.Intcode

object CarePackage {

  def populateBoard(ls: List[Long]): Vector[Tile] = readCommandsRec(new Intcode(ls), Vector[Tile]())._1

  def playGame(ls: List[Long]): Long = {
    val gameProgram = ls.updated(0, 2L)
    readCommandsRec(new Intcode(gameProgram), Vector[Tile]())._2.get
  }

  @scala.annotation.tailrec
  private def readCommandsRec(intcode: Intcode, board: Board): (Board, Option[Long]) = {
    val newCommand = readCommand(intcode)
    newCommand match {
      case Some(Score(_, _, score)) =>
        if (!board.exists(_.id == BLOCK)) (board, Some(score)) else readCommandsRec(intcode, board)
      case Some(Tile(x, y, id)) =>
        val updatedBoard = updateBoard(Tile(x, y, id), board)
        if (id == BALL || id == PADDLE) intcode.provideInput(moveJoystick(updatedBoard))
        readCommandsRec(intcode, updatedBoard)
      case None => (board, None)
    }
  }

  private def moveJoystick(board: Board): Long = {
    val ball = board.find(_.id == BALL)
    val paddle = board.find(_.id == PADDLE)
    if (ball.isEmpty || paddle.isEmpty) 0
    else {
      val ballX = ball.get.x
      val paddleX = paddle.get.x
      if (ballX < paddleX) -1
      else if (ballX == paddleX) 0
      else 1
    }
  }

  private def updateBoard(newTile: Tile, board: Board): Board = {
    val index = board.indexWhere(t => newTile.x == t.x && newTile.y == t.y)
    if (index == -1) board :+ newTile else board.updated(index, newTile)
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
