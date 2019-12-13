package uk.co.hadoopathome.adventofcode19.day13

import uk.co.hadoopathome.adventofcode19.day05.Intcode

object CarePackage {

  def populateTiles(ls: List[Long]): List[Tile] = {
    val intcode = new Intcode(ls)
    readTilesRec(intcode, List[Tile]())
  }

  private def readTilesRec(intcode: Intcode, tiles: List[Tile]): List[Tile] = {
    val newTile = readTile(intcode)
    newTile match {
      case Some(t) => readTilesRec(intcode, tiles :+ t)
      case None => tiles
    }
  }

  private def readTile(intcode: Intcode): Option[Tile] = {
    val (x, isFinished) = intcode.runUntilPause()
    if (isFinished) return None
    val (y, _) = intcode.runUntilPause()
    val (id, _) = intcode.runUntilPause()
    Some(Tile(x.toInt, y.toInt, mapToId(id)))
  }

  private def mapToId(id: Long): Id = id match {
    case 0 => EMPTY
    case 1 => WALL
    case 2 => BLOCK
    case 3 => HORIZONTAL
    case 4 => BALL
  }
}
