package attic


import attic.Day6.ProblemBase
import common.MainBase

object Day6 {
  class ProblemBase extends MainBase(6) {

    override def run(inputFile: List[String]): String ={
      val found = inputFile.head.sliding(14, 1).zipWithIndex.find {
        case (s, _) => { s.toSet.size == s.length }
      }
      (found.get._2 + 14).toString
    }
  }
}

object Day6Problem1 extends ProblemBase

object Day6Problem2 extends ProblemBase {

  override def run(inputFile: List[String]): String =
    ""
}
