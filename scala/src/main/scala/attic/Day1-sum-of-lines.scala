package attic

import common.InputSyntax._
import common.MainBase

private object Day1Problem1 extends Day1ProblemBase(1)

class Day1ProblemBase(day: Int) extends MainBase(day) {

  override def run(inputFile: List[String]): String = {
    val result = inputFile
      .splitVertically("")
      .map(_.map(_.toInt).sum)
      .sortWith((a, b) => a > b)
      .max
    result.toString
  }
}
object Day1Problem2 extends Day1ProblemBase(1) {

  override def run(inputFile: List[String]): String = {
    val result = inputFile
      .splitVertically("")
      .map(_.map(_.toInt).sum)
      .sortWith((a, b) => a > b)
      .take(3)
      .sum
    result.toString
  }
}
