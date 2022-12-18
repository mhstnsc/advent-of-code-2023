package attic

import attic.Day17_2.{State, day, rocks}
import common.InputSyntax.InputSyntax
import common.MainBaseBig

import scala.annotation.tailrec
import scala.util.Try

object Day17_2 {
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

  def printPatch(p: Vector[String]): Unit = {
    println("vvvvvvvvv")
    p.foreach(s => println(s))
    println("\n")
  }

  case class CheckpointKey(
      stackKey: List[Int],
      rockIdx: Int,
      windIdx: Int
  )
  case class CheckpointValue(
      stackHeight: Int,
      usedRocks: Int
  )

  case class State(
      sequence: Vector[Char],
      stack: Vector[String],
      rocks: Vector[Vector[String]],
      rockIdx: Int,
      windIdx: Int,
      checkpoints: Map[CheckpointKey, CheckpointValue],
      discardedStackSize: Int,
      simulatedRocks: Int
  ) {
    def simulate(): State = {
      val patch = generatePatch(rocks.head)

      val stackKey = computeKey(stack)
      val checkpointKey = CheckpointKey(
        stackKey = stackKey,
        rockIdx = rockIdx,
        windIdx = windIdx
      )
      val newCheckpoints = if (checkpoints.contains(checkpointKey)) {
        val foundHistoricEntry = checkpoints(checkpointKey)
        println(s"PastEntry ${checkpointKey} -> ${foundHistoricEntry}")
        println(s"nowEntry ${stack.length}, ${simulatedRocks}")

        val simnulateNumber = BigInt("1000000000000")

        val prefixHeight = foundHistoricEntry.stackHeight

        val simnulateNumberWithoutPrefix = simnulateNumber - foundHistoricEntry.usedRocks
        val periodInRockNumber = simulatedRocks - foundHistoricEntry.usedRocks
        val numberOfRepetitions = simnulateNumberWithoutPrefix / periodInRockNumber
        val repeatedHeight = stack.length - foundHistoricEntry.stackHeight
        val repetitionsHeight = numberOfRepetitions * repeatedHeight

        val numberOfRemaining = simnulateNumberWithoutPrefix % periodInRockNumber

        val suffixHeight = checkpoints.find {
          case (_, value) => value.usedRocks == foundHistoricEntry.usedRocks + numberOfRemaining
        }.map(v => v._2.stackHeight - foundHistoricEntry.stackHeight).get

        val result = prefixHeight + repetitionsHeight + suffixHeight

        throw new Exception(s"${result}")
      } else {
        checkpoints.updated(
          checkpointKey,
          CheckpointValue(stack.size, simulatedRocks)
        )
      }

      val newDepositedState = deposit(DepositState(stack, sequence, patch, overlapSize = 0, windIdx = windIdx))

      State(
        newDepositedState.sequence,
        newDepositedState.stack,
        rocks.appended(rocks.head).drop(1),
        (rockIdx + 1) % rocks.length,
        newDepositedState.windIdx,
        newCheckpoints,
        discardedStackSize,
        simulatedRocks +1,
      )
    }

    def generatePatch(value: Vector[String]): Vector[String] = {
      val prefix = 2
      val suffix = lineWidth - 2 - value.head.length
      value.map(l => "." * prefix + l + "." * suffix) ++ (1 to 3).map(_ => "." * lineWidth)
    }

    def print(): Unit = {
      stack.foreach(v => println(v))
      println("\n")
    }
  }

  def computeKey(stack: Vector[String]): List[Int] = {
    @tailrec
    def collapseStackImpl(stack: Vector[String], acc: List[Int], closed: List[Boolean]): List[Int] =
      if (stack.isEmpty) {
        acc
      } else {
        val topLine = stack.head
        val (newAcc, newClosed) = (acc zip topLine zip closed).map { case ((accV, lineV), isClosed) =>
          if (isClosed) {
            (accV, isClosed)
          } else {
            if (lineV == '.') {
              val extra = if (stack.size == 1) 1 else 0
              (accV + 1 + extra, false)
            } else {
              (accV + 1, true)
            }
          }
        }.unzip
        collapseStackImpl(stack.drop(1), newAcc, newClosed)
      }
    collapseStackImpl(stack, List.fill(lineWidth)(0), List.fill(lineWidth)(false))
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
      overlapSize: Int,
      windIdx: Int
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

      DepositState(
        stack,
        sequence.appended(sequence.head).drop(1),
        windActionedPatch,
        overlapSize,
        (windIdx + 1) % sequence.size
      )
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
          DepositState(newStack, sequence, Vector.empty, 0, windIdx)
        }

        if (stack.size == overlapSize) {
          // we perform merge because we hit the bottom
          mergeIntoStack()
        } else {
          val newOverlapSize = overlapSize + 1
          val stackPatch     = extractStackPatch(newOverlapSize, patch.size)

          // check if movement is possible
          if (!isOverlapping(stackPatch, patch)) {
            DepositState(stack, sequence, patch, newOverlapSize, windIdx)
          } else {
            mergeIntoStack()
          }
        }
      } else {
        require(overlapSize == 0)
        DepositState(stack, sequence, patch.dropRight(1), overlapSize, windIdx)
      }
  }
}

object Day17_Problem2 extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val airSequence = inputFile.head.toVector

    val emptyState =
      State(airSequence, Vector.empty, rocks, rockIdx = 0, windIdx = 0, Map.empty, discardedStackSize = 0, simulatedRocks = 0)

    val repeatPeriod = airSequence.size * rocks.size * 6
    val state = Try(
      (1 to repeatPeriod).foldLeft(emptyState) { (acc, _) =>
      acc.simulate()
    }).toEither.swap.toOption.map(_.getMessage).get
    state
  }
}
