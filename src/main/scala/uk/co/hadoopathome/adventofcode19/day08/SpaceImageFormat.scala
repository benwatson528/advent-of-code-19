package uk.co.hadoopathome.adventofcode19.day08

object SpaceImageFormat {
  type Layer = List[Int]

  def checkCorruption(ls: List[Int], width: Int, height: Int): Int = {
    val layers = ls.grouped(width * height).toList
    val fewestZeros = layers.reduceLeft((a, b) => if (a.count(_ == 0) < b.count(_ == 0)) a else b)
    fewestZeros.count(_ == 1) * fewestZeros.count(_ == 2)
  }

  def decode(ls: Layer, width: Int, height: Int): Layer = {
    val layers = ls.grouped(width * height).toList
    val superimposed = for (i <- layers.head.indices) yield layers.map(_ (i))
    superimposed.map(_.find(_ != 2).getOrElse(2)).toList
  }
}
