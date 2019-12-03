package uk.co.hadoopathome.adventofcode19

package object day03 {

  type Coord = (Int, Int)

  sealed trait Direction {
    def direction: String
  }

  case class Command(direction: Direction, magnitude: Int)

  case object UP extends Direction {
    val direction = "U"
  }

  case object DOWN extends Direction {
    val direction = "D"
  }

  case object LEFT extends Direction {
    val direction = "L"
  }

  case object RIGHT extends Direction {
    val direction = "R"
  }

}
