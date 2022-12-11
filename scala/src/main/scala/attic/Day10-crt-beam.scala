package attic

import attic.Day10.parse
import common.MainBase

object Day10 {
  case class Command(cycleCost: Int, amount: Int)

  def parse(inputFile: List[String]): List[Command] =
    inputFile.map { s =>
      if (s == "noop") Command(1, 0)
      else {
        Command(2, s.split(" ")(1).toInt)
      }
    }
}

object Day10Problem1 extends MainBase(10) {
  override def run(inputFile: List[String]): String = {
    val parsed = parse(inputFile)
    case class FoldState(xValue: Int, xHistory: Vector[Int])
    val emptyState = FoldState(1, xHistory = Vector(1))
    val computedState = parsed.foldLeft(emptyState) { case (acc, command) =>
      val newValue   = acc.xValue + command.amount
      val newHistory = acc.xHistory ++ ((0 until command.cycleCost).map(_ => acc.xValue).toVector)
      FoldState(newValue, newHistory)
    }
    println(computedState.xHistory.mkString("\n"))
    val values = List(20, 60, 100, 140, 180, 220).map { i =>
      computedState.xHistory.apply(i) * i
    }.sum
    values.toString
  }
}

object Day10Problem2 extends MainBase(10) {
  override def run(inputFile: List[String]): String = {
    val parsed = parse(inputFile)
    case class FoldState(xValue: Int, xHistory: Vector[Int])
    val emptyState = FoldState(1, xHistory = Vector(1))
    val computedState = parsed.foldLeft(emptyState) { case (acc, command) =>
      val newValue   = acc.xValue + command.amount
      val newHistory = acc.xHistory ++ ((0 until command.cycleCost).map(_ => acc.xValue).toVector)
      FoldState(newValue, newHistory)
    }

    val image = for {
      beamPos <- 0 until 240
    } yield {
      val historyPos = beamPos % 40
      val xValue = computedState.xHistory(beamPos + 1)
      if(historyPos >= xValue - 1 && historyPos <= xValue + 1) '#'
      else '.'
    }
    "\n" + image.sliding(40, 40).map(l => l.mkString("")).mkString("\n")
  }

}
