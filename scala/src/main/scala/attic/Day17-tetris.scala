package attic

import attic.Day17._
import common.InputSyntax.InputSyntax
import common.{MainBaseBig, MainBaseSmall}

import scala.annotation.tailrec

object Day17 {
  val day = 17

  val rocks: Vector[Vector[String]] =
    """
      |####
      |
      |.#.
      |###
      |.#.
      |
      |..#
      |..#
      |###
      |
      |#
      |#
      |#
      |#
      |
      |##
      |##
      |""".stripMargin.split("\n").toList.splitVertically("").map(_.toVector).toVector

  def parse(input: List[String]): List[List[String]] =
    input.splitVertically("")

  val lineWidth = 7

  def printPatch(p: Vector[String]) = {
    println("vvvvvvvvv")
    p.foreach(s => println(s))
    println("\n")
  }

  case class State(
      sequence: Vector[Char],
      stack: Vector[String],
      rocks: Vector[Vector[String]]
  ) {
    def simulate: State = {

      val patch = generatePatch(rocks.head)
//      println("Falling match")
//      printPatch(patch)

      val newDepositedState = deposit(DepositState(stack, sequence, patch, 0))

      State(newDepositedState.sequence, newDepositedState.stack, rocks.appended(rocks.head).drop(1))
    }

    def generatePatch(value: Vector[String]): Vector[String] = {
      val prefix = 2
      val suffix = lineWidth - 2 - value.head.size
      value.map(l => "." * prefix + l + "." * suffix) ++ (1 to 3).map(_ => "." * lineWidth)
    }

    def print(): Unit = {
      stack.foreach(v => println(v))
      println("\n")
    }
  }

  @tailrec
  def deposit(depositState: DepositState): DepositState =
    if (depositState.patch.isEmpty) {
      depositState
    } else {
      deposit(depositState.windAction().fallAction())
    }

  case class DepositState(
      stack: Vector[String],
      sequence: Vector[Char],
      patch: Vector[String],
      overlapSize: Int
  ) {
    def shiftPatch(patch: Vector[String], direction: Char): Vector[String] =
      direction match {
        case '>' =>
          if (patch.exists(_.last == '#')) patch
          else {
            patch.map(l => "." + l.dropRight(1))
          }
        case '<' =>
          if (patch.exists(_.head == '#')) patch
          else patch.map(l => l.drop(1) + ".")
      }

    def windAction(): DepositState = {
      val stackOverlap = extractStackPatch(overlapSize, patch.size)

      val windActionedPatch = {
        val shiftedPatch = shiftPatch(patch, sequence.head)
        if (isOverlapping(shiftedPatch, stackOverlap)) {
          patch
        } else
          shiftedPatch
      }

      DepositState(stack, sequence.appended(sequence.head).drop(1), windActionedPatch, overlapSize)
    }

    def extractStackPatch(overlapSize: Int, patchSize: Int): Vector[String] =
      if (overlapSize == 0) {
        (1 to patchSize).map(_ => "." * lineWidth).toVector
      } else {
        if (overlapSize >= patch.size) {
          stack.slice(overlapSize - patch.size, overlapSize)
        } else {
          ((1 to patchSize - overlapSize).map(_ => "." * lineWidth) ++ stack.take(overlapSize)).toVector
        }
      }

    def isOverlapping(stackPatch: Vector[String], patch: Vector[String]): Boolean = {
      require(stackPatch.length == patch.length)
      (stackPatch zip patch).flatMap(v => v._1 zip v._2).exists { case (a, b) =>
        a == '#' && b == '#'
      }
    }

    def merge(a: Vector[String], b: Vector[String]): Vector[String] = {
//      println("merging")
//      printPatch(a)
//      printPatch(b)
      require(a.length == b.length, s"$a == $b")
      require(a.head.length == b.head.length, s"${a.head} == ${b.head}")
      val r = (a zip b).map { case (aLine, bLine) =>
        (aLine zip bLine).map {
          case ('.', '.') => '.'
          case ('#', '.') => '#'
          case ('.', '#') => '#'
          case ('#', '#') => throw new Exception("this should not happen")
          case _          => throw new Exception("bulllshit")
        }.mkString("")
      }
//      printPatch(r)
      r
    }

    def fallAction(): DepositState =
      if (patch.last.contains('#')) {
        def mergeIntoStack(): DepositState = {
          val mergeStackPatch = extractStackPatch(overlapSize, patch.size)
          val merged          = merge(mergeStackPatch, patch)
          val newStack = if (overlapSize < patch.size) {
            merged ++ stack.drop(overlapSize)
          } else {
            stack.patch(overlapSize - merged.size, merged, merged.size)
          }
          DepositState(newStack, sequence, Vector.empty, 0)
        }

        if (stack.size == overlapSize) {
          // we perform merge because we hit the bottom
          mergeIntoStack()
        } else {
          val newOverlapSize = overlapSize + 1
          val stackPatch     = extractStackPatch(newOverlapSize, patch.size)

          // check if movement is possible
          if (!isOverlapping(stackPatch, patch)) {
            DepositState(stack, sequence, patch, newOverlapSize)
          } else {
            mergeIntoStack()
          }
        }
      } else {
        require(overlapSize == 0)
        DepositState(stack, sequence, patch.dropRight(1), overlapSize)
      }
  }
}

object Day17_Problem1 extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val airSequence = inputFile.head.toVector

    val emptyState = State(airSequence, Vector.empty, rocks)
    val state = (1 to 2022).foldLeft(emptyState) { (acc, step) =>
      println(s"----> $step")
      val newAcc = acc.simulate
//      newAcc.print()
      newAcc
    }
//    state.print()

    state.stack.size.toString

  }
}

object Day17_Problem1_Small extends MainBaseSmall(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)

    ""
  }
}

object Day17_Problem1_Big extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)
    ""
  }
}

object Day17_Problem2_Small extends MainBaseSmall(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)
    ""
  }
}

object Day17_Problem2_Big extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)
    ""
  }
}
