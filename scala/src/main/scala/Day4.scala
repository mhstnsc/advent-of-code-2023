import common.{MainBase, MainBaseSmall}

private abstract class Day4Base extends MainBase(4) {}

private object Day4Problem1 extends Day4Base {
  override def run(inputFile: List[String]): String =
    inputFile.map { l =>
      l.split("[:|]").toList match {
        case _ +: winning +: all +: Nil =>
          val winningNumbers = winning.trim.split(" +").map(_.toInt).toSet
          val allNumbers     = all.trim.split(" +").map(_.toInt)

          val foundNumbers = allNumbers.zipWithIndex.filter { case (number, idx) =>
            winningNumbers.contains(number)
          }.map { case (number, idx) =>
            number
          }

          if (foundNumbers.isEmpty)
            0
          else
            Math.pow(2, foundNumbers.length - 1)
      }
    }.sum.toString
}

private object Day4Problem2 extends Day4Base {
  def parseNumbers(l: String): (Set[Int], List[Int]) =
    l.split("[:|]").toList match {
      case _ +: winning +: all +: Nil =>
        (winning.trim.split(" +").map(_.toInt).toSet, all.trim.split(" +").map(_.toInt).toList)
    }

  override def run(inputFile: List[String]): String = {
    case class State(totalCards: Int, cachedState: Map[Int, Int])
    val startingState = State(0, Map.empty)

    val finalState = inputFile.zipWithIndex.foldRight(startingState) { case ((l, lineIdx), state) =>
      val (winning, all) = parseNumbers(l)

      val foundNumbers = all.filter(number => winning.contains(number))

      val newCardIdxs = (1 to foundNumbers.length).map(v => lineIdx + v).filter(_ < inputFile.length)

      val newCardRewards = newCardIdxs.map(v => state.cachedState(v))

      val newState = State(
        state.totalCards + 1 + newCardRewards.sum,
        cachedState = state.cachedState + (lineIdx -> (1 + newCardRewards.sum))
      )

      newState
    }

    finalState.totalCards.toString
  }

}
