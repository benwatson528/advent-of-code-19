package uk.co.hadoopathome.adventofcode19.day1

import scala.annotation.tailrec

object FrequencyLooper {
    def frequencyLooper(ls: List[Int]): Int = {
        frequencyLooperRec(ls, 0, Set[Int](), ls)
    }

    @tailrec
    def frequencyLooperRec(ls: List[Int], sum: Int, seen: Set[Int], original: List[Int]): Int = {
        ls match {
            case x :: _ if seen.contains(sum + x) => sum + x
            case x :: Nil => frequencyLooperRec(original, sum + x, seen + (sum + x), original)
            case x :: xx => frequencyLooperRec(xx, sum + x, seen + (sum + x), original)
        }
    }
}
