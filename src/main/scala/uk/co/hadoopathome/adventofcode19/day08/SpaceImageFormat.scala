package uk.co.hadoopathome.adventofcode19.day08

object SpaceImageFormat {
  type Layer = List[Int]

  def checkCorruption(ls: Layer, width: Int, height: Int): Int = {
    val pixelsPerLayer = width * height
    val layers = ls.grouped(pixelsPerLayer)
    val fewestZeros = layers.reduceLeft(minZeros)
    fewestZeros.count(_ == 1) * fewestZeros.count(_ == 2)
  }

  private def minZeros(ls1: Layer, ls2: Layer): Layer = if (ls1.count(_ == 0) < ls2.count(_ == 0)) ls1 else ls2
}
