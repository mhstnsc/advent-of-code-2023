package attic

import attic.Day2.ProblemBase
import common.MainBase

object Day2 {
  class ProblemBase extends MainBase(2) {

    def stringToValue(s: String) =
      s match {
        case "A" => 1
        case "B" => 2
        case "C" => 3
        case "X" => 1
        case "Y" => 2
        case "Z" => 3
      }

    override def run(inputFile: List[String]): String =
      inputFile
        .map(_.split(" "))
        .map { v =>
          val opponentValue = stringToValue(v(0))
          val meValue = stringToValue(v(1))
          if(opponentValue == meValue) {
            3 + meValue
          } else if(List((1, 3), (3, 2), (2, 1)).contains((opponentValue, meValue))) {
            meValue
          } else
            6 + meValue
        }
        .sum
        .toString
  }
}

object Day2Problem1 extends ProblemBase

object Day2Problem2 extends ProblemBase {

  def predict(opponentValue: Int, outcome: String) =
    if(outcome == "X") { // loose
      if(opponentValue == 1) 3 else opponentValue - 1
    } else if(outcome == "Y") {
      opponentValue
    } else if(outcome == "Z")
      if(opponentValue == 3) 1 else opponentValue + 1
    else
      throw new Exception("bla")

  override def run(inputFile: List[String]): String =
    inputFile
      .map(_.split(" "))
      .map { v =>
        val opponentValue = stringToValue(v(0))
        val meValue       = predict(opponentValue, v(1))
          if (opponentValue == meValue) {
          3 + meValue
        } else if (List((1, 3), (3, 2), (2, 1)).contains((opponentValue, meValue))) {
          meValue
        } else
          6 + meValue
      }
      .sum
      .toString
}
