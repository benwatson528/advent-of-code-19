package uk.co.hadoopathome.adventofcode19.day1

object ChronalCalibration {
    def sumNumbers(ls: List[Int]): Int = {
        sumNumbersRec(ls, 0)
    }

    def sumNumbersRec(ls: List[Int], sum: Int): Int = {
        ls match {
            case x :: xx => sumNumbersRec(xx, sum + x)
            case _ => sum
        }
    }
}
