package uk.co.hadoopathome.adventofcode19.day1

import org.scalatest.FunSuite

import scala.io.Source

class ChronalCalibrationTest extends FunSuite {
    test("sumNumbers +1, +1, +1") {
        val input = List(1, 1, 1)
        assert(3 === ChronalCalibration.sumNumbers(input))
    }

    test("sumNumbers +1, +1, -2") {
        val input = List(1, 1, -2)
        assert(0 === ChronalCalibration.sumNumbers(input))
    }

    test("sumNumbers -1, -2, -3") {
        val input = List(-1, -2, -3)
        assert(-6 === ChronalCalibration.sumNumbers(input))
    }

    test("sumNumbers real") {
        val input = Source.fromResource("day1/input.txt").getLines.toList
            .map(_.toString.toInt)
        assert(435 === ChronalCalibration.sumNumbers(input))
    }
}
