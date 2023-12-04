package attic

import common.MainBase

case class Game(id: Int, draws: List[Map[String, Int]])

private abstract class Day2Base extends MainBase(2) {
  def parseDraw(s: String): Map[String, Int] = {
    val re = "([0-9]+) ([a-z]+)".r
    re.findAllMatchIn(s).toList.map(m => m.group(2) -> m.group(1).toInt).toMap
  }

  def parseInput(input: List[String]): List[Game] =
    input.map(
      _.split(":").toList match {
        case game +: draws +: Nil =>
          val gameId = game.split(" ").last.toInt
          val x = draws
            .split(";")
            .toList
            .map(parseDraw)
          Game(gameId, x)
      }
    )

  val maxDraw: Map[String, Int] = Map(
    "red"   -> 12,
    "green" -> 13,
    "blue"  -> 14
  )
}

private object Day2Problem1 extends Day2Base {
  override def run(inputFile: List[String]): String = {
    val games = parseInput(inputFile)
    games.filterNot { g =>
      g.draws.exists { m =>
        m.exists { case (color, amount) => amount > maxDraw(color) }
      }
    }.map(_.id).sum.toString
  }
}

private object Day2Problem2 extends Day2Base {
  override def run(inputFile: List[String]): String = {
    val games = parseInput(inputFile)
    games.map { g =>
      g.draws.foldRight(Map.empty[String, Int]) { case (acc, v) =>
        val colors = acc.keySet ++ v.keySet
        colors.toList.map(c => c -> Math.max(acc.getOrElse(c, 0), v.getOrElse(c, 0))).toMap
      }
    }.map { m =>
      m.values.foldRight(1) { case (acc, v) => acc * v }
    }.sum.toString
  }
}
