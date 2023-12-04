package attic

import common.MainBase

private object Day1Problem1 extends Day1ProblemBase(1)

class Day1ProblemBase(day: Int) extends MainBase(day) {

  override def run(inputFile: List[String]): String = {

    def extractNumber(s: String): Int = {

      val processed = {
        for {
          i <- 0 until s.length
        } yield {
          val slice = s.slice(i, s.length)
          val mappings = Map(
            "one" -> 1,
            "two" -> 2,
            "three" -> 3,
            "four" -> 4,
            "five" -> 5,
            "six" -> 6,
            "seven" -> 7,
            "eight" -> 8,
            "nine" -> 9,
            "zero" -> 0,
            "1" -> 1,
            "2" -> 2,
            "3" -> 3,
            "4" -> 4,
            "5" -> 5,
            "6" -> 6,
            "7" -> 7,
            "8" -> 8,
            "9" -> 9,
            "0" -> 0
          )

          mappings.map {
            case (searched, result) => Option.when(slice.startsWith(searched))(result)
          }.flatten.headOption
        }
      }.flatten

      s"${processed.head}${processed.last}".toInt
    }

    val result = inputFile
      .map(extractNumber)
      .sum
    result.toString
  }
}