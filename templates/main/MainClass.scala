package uk.co.hadoopathome.adventofcode19.${internal_day}

object MainClass {
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
