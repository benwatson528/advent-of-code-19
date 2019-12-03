package uk.co.hadoopathome.adventofcode19.day03

object CrossedWires {

  private val START_POSITION = new Coord(0, 0)

  def crossWires(wire1: List[String], wire2: List[String]): Int = {
    val wire1Coords = travel(wire1).toSet
    val wire2Coords = travel(wire2).toSet
    val intersectedCoords = wire1Coords.intersect(wire2Coords)
    intersectedCoords.filterNot(c => c == START_POSITION).map(getManhattanDistance).min
  }

  def travel(wire: List[String]): IndexedSeq[Coord] =
    travelRec(wire.map(convertToCommand), IndexedSeq[Coord](START_POSITION))

  def convertToCommand(rawCommand: String): Command = {
    val (rawDirection, rawMagnitude) = (rawCommand.head, rawCommand.tail)
    val direction = rawDirection match {
      case x if x == 'U' => UP
      case x if x == 'D' => DOWN
      case x if x == 'L' => LEFT
      case x if x == 'R' => RIGHT
    }
    Command(direction, rawMagnitude.toInt)
  }

  @scala.annotation.tailrec
  def travelRec(xs: List[Command], travelledCoords: IndexedSeq[Coord]): IndexedSeq[Coord] = xs match {
    case x :: xs => travelRec(xs, travelledCoords ++ drawLine(travelledCoords.last, x))
    case _ => travelledCoords
  }

  def drawLine(startCoord: Coord, command: Command): IndexedSeq[Coord] = {
    command.direction match {
      case UP => for (i <- startCoord._2 + 1 to startCoord._2 + command.magnitude) yield new Coord(startCoord._1, i)
      case DOWN => for (i <- startCoord._2 - 1 to startCoord._2 - command.magnitude by -1) yield new Coord(startCoord._1, i)
      case LEFT => for (i <- startCoord._1 - 1 to startCoord._1 - command.magnitude by -1) yield new Coord(i, startCoord._2)
      case RIGHT => for (i <- startCoord._1 + 1 to startCoord._1 + command.magnitude) yield new Coord(i, startCoord._2)
    }
  }

  def getManhattanDistance(coord: Coord): Int = coord._1.abs + coord._2.abs
}
