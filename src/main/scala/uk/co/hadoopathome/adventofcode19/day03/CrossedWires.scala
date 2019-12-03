package uk.co.hadoopathome.adventofcode19.day03

object CrossedWires {

  private val START_POSITION = new Coord(0, 0)

  def crossWiresManhattan(wire1: List[String], wire2: List[String]): Int = {
    val wire1Coords = travel(wire1)
    val wire2Coords = travel(wire2)
    findIntersections(wire1Coords, wire2Coords).map(getManhattanDistance).min
  }

  def crossWiresShortest(wire1: List[String], wire2: List[String]): Int = {
    val wire1Coords = travel(wire1)
    val wire2Coords = travel(wire2)
    val intersections = findIntersections(wire1Coords, wire2Coords)
    intersections.map(i => wire1Coords.indexOf(i) + wire2Coords.indexOf(i)).min
  }

  private def findIntersections(wire1Coords: IndexedSeq[Coord], wire2Coords: IndexedSeq[Coord]): IndexedSeq[Coord] =
    wire1Coords.intersect(wire2Coords).filterNot(c => c == START_POSITION)

  private def travel(wire: List[String]): IndexedSeq[Coord] =
    travelRec(wire.map(c => Command(c.head, c.tail.toInt)), IndexedSeq[Coord](START_POSITION))

  @scala.annotation.tailrec
  private def travelRec(xs: List[Command], travelledCoords: IndexedSeq[Coord]): IndexedSeq[Coord] = xs match {
    case x :: xs => travelRec(xs, travelledCoords ++ drawLine(travelledCoords.last, x))
    case _ => travelledCoords
  }

  private def drawLine(startCoord: Coord, command: Command): IndexedSeq[Coord] = {
    command.direction match {
      case x if x == 'U' =>
        for (i <- startCoord._2 + 1 to startCoord._2 + command.magnitude) yield new Coord(startCoord._1, i)
      case x if x == 'D' =>
        for (i <- startCoord._2 - 1 to startCoord._2 - command.magnitude by -1) yield new Coord(startCoord._1, i)
      case x if x == 'L' =>
        for (i <- startCoord._1 - 1 to startCoord._1 - command.magnitude by -1) yield new Coord(i, startCoord._2)
      case x if x == 'R' =>
        for (i <- startCoord._1 + 1 to startCoord._1 + command.magnitude) yield new Coord(i, startCoord._2)
    }
  }

  private def getManhattanDistance(coord: Coord): Int = coord._1.abs + coord._2.abs
}
