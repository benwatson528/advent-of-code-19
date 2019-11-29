package uk.co.hadoopathome.adventofcode19.day1

import org.scalatest.FunSuite

import scala.io.Source

class FrequencyLooperTest extends FunSuite {
    test("frequencyLooper +3, +3, +4, -2, -4") {
        val input = List(3, 3, 4, -2, -4)
        assert(10 === FrequencyLooper.frequencyLooper(input))
    }

    test("frequencyLooper -6, +3, +8, +5, -6") {
        val input = List(-6, 3, 8, 5, -6)
        assert(5 === FrequencyLooper.frequencyLooper(input))
    }

    test("frequencyLooper +7, +7, -2, -7, -4") {
        val input = List(7, 7, -2, -7, -4)
        assert(14 === FrequencyLooper.frequencyLooper(input))
    }

    test("frequencyLooper real") {
        val input = Source.fromResource("day1/input.txt").getLines.toList
            .map(_.toString.toInt)
        assert(245 === FrequencyLooper.frequencyLooper(input))
    }
}
