package uk.co.hadoopathome.adventofcode19.${internal_day}

import org.scalatest.FunSuite

import scala.io.Source

class ${internal_class_name}Test extends FunSuite {
  test("sumNumbers +1, +1, +1") {
    val input = List(1, 1, 1)
    assert(3 === ${internal_class_name}.sumNumbers(input))
  }

  test("sumNumbers real") {
    val input = Source.fromResource("${internal_day}/input.txt").getLines.toList
        .map(_.toString.toInt)
    assert(435 === ${internal_class_name}.sumNumbers(input))
  }
}
