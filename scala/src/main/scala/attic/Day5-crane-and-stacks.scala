package attic

import attic.Day5.ProblemBase
import common.InputSyntax.InputSyntax
import common.MainBase

object Day5 {
  class ProblemBase extends MainBase(5) {

    type Config = Vector[List[Char]]

    case class Command(amount: Int, from: Int, to: Int)

    def parseConfig(lines: List[String]): Config = {
      val numStacks     = lines.last.split(" ").count(_.nonEmpty)
      val stackContents = lines.dropRight(1).reverse
      val emptyConfig   = (0 until numStacks).map(_ => List.empty[Char]).toVector
      val r = stackContents.foldLeft(emptyConfig) { (acc, l) =>
        val toAddList = for {
          i <- 0 until numStacks
          charIndex = i * 4 + 1
          char = Option.when(charIndex < l.length)(l(charIndex)).filter(_ != ' ')
        } yield char
        val newAcc = (acc zip toAddList).map { case (l, maybeC) => maybeC.map(c => c +: l).getOrElse(l) }
        newAcc
      }
      r
    }

    def parseCommands(lines: List[String]): List[Command] = {
      val re = "move (\\d+) from (\\d+) to (\\d+)".r
      lines.map { case re(amount, from, to) => Command(amount.toInt, from.toInt, to.toInt) }
    }

    def transform(config: Config, command: Command): Config = {
      val contents: List[Char]   = config(command.from - 1).take(command.amount).reverse
      val updatedOld: List[Char] = config(command.from - 1).drop(command.amount)
      val updatedNew: List[Char] = contents ++ config(command.to - 1)

      config
        .updated(command.from - 1, updatedOld)
        .updated(command.to - 1, updatedNew)
    }

    override def run(inputFile: List[String]): String = {
      val (config, commands) = inputFile.splitVertically("") match {
        case initialConfigLines +: commandLines +: Nil => (parseConfig(initialConfigLines), parseCommands(commandLines))
      }
      val result = commands.foldLeft(config) { case (acc, c) => transform(acc, c) }
      result.flatMap(l => l.headOption).mkString("")
    }
  }
}

object Day5Problem1 extends ProblemBase

object Day5Problem2 extends ProblemBase {
  def transform2(config: Config, command: Command): Config = {
    val contents: List[Char]   = config(command.from - 1).take(command.amount)
    val updatedOld: List[Char] = config(command.from - 1).drop(command.amount)
    val updatedNew: List[Char] = contents ++ config(command.to - 1)

    config
      .updated(command.from - 1, updatedOld)
      .updated(command.to - 1, updatedNew)
  }

  override def run(inputFile: List[String]): String = {
    val (config, commands) = inputFile.splitVertically("") match {
      case initialConfigLines +: commandLines +: Nil => (parseConfig(initialConfigLines), parseCommands(commandLines))
    }
    val result = commands.foldLeft(config) { case (acc, c) => transform2(acc, c) }
    result.flatMap(l => l.headOption).mkString("")
  }

}
