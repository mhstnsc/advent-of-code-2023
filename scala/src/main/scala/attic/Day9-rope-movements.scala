package attic

import attic.Day9.{Point, ProblemBase, Rope, runCommands}
import common.MainBase

object Day9 {

  case class Point(x: Int, y: Int)

  def moveTail(head: Point, tail: Point): Point = {
    val difx = head.x - tail.x
    val dify = head.y - tail.y
    if (math.abs(difx) >= 2 || math.abs(dify) >= 2) {
      val stepx = if (difx != 0) difx / math.abs(difx) else 0
      val stepy = if (dify != 0) dify / math.abs(dify) else 0
      Point(tail.x + stepx, tail.y + stepy)
    } else {
      tail
    }
  }

  case class Rope(head: Point, tail: Vector[Point]) {
    def move(dx: Int, dy: Int): Rope = {
      val newHead = Point(head.x + dx, head.y + dy)
      case class TailMoveState(myHead: Point, newTail: Vector[Point])
      val newTail = tail.foldLeft(TailMoveState(newHead, Vector.empty)) { case (acc, knot) =>
        val newKnot = moveTail(acc.myHead, knot)
        TailMoveState(
          newKnot,
          acc.newTail.appended(newKnot)
        )
      }
      Rope(newHead, newTail.newTail)
    }
  }

  case class Command(dir: Char, amount: Int)
  case class CommandResult(state: Rope, recording: Vector[Point])

  def runCommands(rope: Rope, commands: List[Command]): CommandResult =
    commands.foldLeft(CommandResult(rope, Vector(Point(0, 0)))) { case (state, command) =>
      Range(0, command.amount).foldLeft(state) { case (accState, _) =>
        val newState = command.dir match {
          case 'R' => accState.state.move(1, 0)
          case 'L' => accState.state.move(-1, 0)
          case 'U' => accState.state.move(0, 1)
          case 'D' => accState.state.move(0, -1)
        }
        val newRecording = accState.recording.appended(newState.tail.last)
        CommandResult(newState, newRecording)
      }
    }

  class ProblemBase extends MainBase(9) {
    def parse(inputLines: List[String]): List[Command] =
      inputLines.map { l =>
        val a = l.split(" ")
        Command(a(0)(0), a(1).toInt)
      }

    override def run(inputFile: List[String]): String = {
      val rope2         = Rope(Point(0, 0), Vector(Point(0, 0)))
      val commandOutput = runCommands(rope2, parse(inputFile))
      commandOutput.recording.toSet.size.toString
    }
  }
}

object Day9Problem1 extends ProblemBase

object Day9Problem2 extends ProblemBase {

  override def run(inputFile: List[String]): String = {
    val initialRope   = Rope(Point(0, 0), Range(0, 9).map(_ => Point(0, 0)).toVector)
    val commandOutput = runCommands(initialRope, parse(inputFile))
    commandOutput.recording.toSet.size.toString
  }

}
