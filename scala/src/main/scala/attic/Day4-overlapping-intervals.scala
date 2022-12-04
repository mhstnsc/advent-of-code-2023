package attic

import attic.Day4.ProblemBase
import common.MainBase

object Day4 {
  class ProblemBase(day: Int) extends MainBase(day) {

    def isFullyContained(r1: Array[Int], r2: Array[Int]): Boolean =
      (r1(0) <= r2(0) && r1(1) >= r2(1)) || (r2(0) <= r1(0) && r2(1) >= r1(1))

    def isOverlapping(r1: Array[Int], r2: Array[Int]): Boolean =
      !(r1(0) > r2(1) || r1(1) < r2(0))

    override def run(inputFile: List[String]): String =
      inputFile
        .map(s => s.split(",").map(s => s.split("-").map(_.toInt)))
        .count { ranges =>
          isFullyContained(ranges(0), ranges(1))
        }
        .toString
  }
}

object Day4Problem1 extends ProblemBase(4)

object Day4Problem2 extends ProblemBase(4) {

  override def run(inputFile: List[String]): String =
    inputFile
      .map(s => s.split(",").map(s => s.split("-").map(_.toInt)))
      .count { ranges =>
        isOverlapping(ranges(0), ranges(1))
      }
      .toString
}
