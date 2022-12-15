package attic

import attic.Day14.{parse, render}
import common.FileUtils.writeFile
import common.Terminal.{ANSI_BLUE, ANSI_RED, ANSI_RESET, ANSI_WHITE}
import common.{MainBase, Matrix}

import scala.annotation.tailrec

object Day14 {

  val rockChar    = '#'
  val sandChar    = 'o'
  val nothingChar = '.'

  def extrapolate(tuple1: (Int, Int), tuple2: (Int, Int)): Set[(Int, Int)] = {
    for {
      c <- math.min(tuple1._1, tuple2._1) to math.max(tuple2._1, tuple1._1)
      l <- math.min(tuple1._2, tuple2._2) to math.max(tuple2._2, tuple1._2)
    } yield (c, l)
  }.toSet

  def parse(inputFile: List[String]): Map[(Int, Int), Char] =
    inputFile
      .flatMap(l =>
        l.split(" -> ")
          .map { l =>
            val points = l.split(",").map(_.toInt)
            (points(0), points(1))
          }
          .sliding(2)
          .map(arr => extrapolate(arr(0), arr(1)))
      )
      .foldLeft(Map.empty[(Int, Int), Char]) { case (acc, v) =>
        v.foldLeft(acc) { case (acc, point) =>
          acc.updated(point, rockChar)
        }
      }

  def render(rockSet: Map[(Int, Int), Char]): String = {

    val numLines = rockSet.keys.map(_._2).max + 1
    val numCols  = rockSet.keys.map(_._1).max - rockSet.keys.map(_._1).min + 1

    val emptyMatrix = Matrix.filled[Char](numLines, numCols, nothingChar)

    val minCol = rockSet.keys.map(_._1).min

    val updatedMatrix = rockSet.foldLeft(emptyMatrix) { case (acc, ((col, line), char)) =>
      acc.updated(line, col - minCol, char)
    }

    updatedMatrix.mkString(
      "",
      (v, p) =>
        if (v == rockChar) {
          s"${ANSI_RED}$v$ANSI_RESET"
        } else if (v == sandChar) {
          s"${ANSI_BLUE}$v$ANSI_RESET"
        } else
          s"${ANSI_WHITE}$v$ANSI_RESET"
    )
  }
}

object Day14Problem1 extends MainBase(14) {
  @tailrec
  def simulateOneLeaking(
      sand: (Int, Int),
      input: Map[(Int, Int), Char],
      minDepth: Int
  ): (Boolean, Map[(Int, Int), Char]) = {
    val down      = (sand._1, sand._2 + 1)
    val downLeft  = (sand._1 - 1, sand._2 + 1)
    val downRight = (sand._1 + 1, sand._2 + 1)

    if (sand._2 > minDepth) {
      (false, input)
    } else {
      if (!input.contains(down)) {
        simulateOneLeaking(down, input, minDepth)
      } else {
        if (!input.contains(downLeft)) {
          simulateOneLeaking(downLeft, input, minDepth)
        } else if (!input.contains(downRight)) {
          simulateOneLeaking(downRight, input, minDepth)
        } else {
          (true, input.updated(sand, 'o'))
        }
      }
    }
  }

  @tailrec
  def pourUntilLeaking(input: Map[(Int, Int), Char], minDepth: Int, pouredAmount: Int): Int = {
    val startPoint               = (500, 0)
    val (wasUpdated, updatedSet) = simulateOneLeaking(startPoint, input, minDepth)
    if (wasUpdated) {
      pourUntilLeaking(updatedSet, minDepth, pouredAmount + 1)
    } else {
      println("\n" + render(input))
      writeFile("result.txt", render(input))
      pouredAmount
    }
  }

  override def run(inputFile: List[String]): String = {
    val inputs   = parse(inputFile)
    val minDepth = inputs.keys.map(_._2).max

    val count = pourUntilLeaking(inputs, minDepth, 0)
    count.toString
  }
}

object Day14Problem2 extends MainBase(14) {

  @tailrec
  def simulateOne(
      sand: (Int, Int),
      set: Map[(Int, Int), Char],
      minDepth: Int
  ): Map[(Int, Int), Char] = {
    val down      = (sand._1, sand._2 + 1)
    val downLeft  = (sand._1 - 1, sand._2 + 1)
    val downRight = (sand._1 + 1, sand._2 + 1)

    if (sand._2 == minDepth + 1) {
      set.updated(sand, 'o') // deposit the sand
    } else {
      if (!set.contains(down)) {
        simulateOne(down, set, minDepth)
      } else {
        if (!set.contains(downLeft)) {
          simulateOne(downLeft, set, minDepth)
        } else if (!set.contains(downRight)) {
          simulateOne(downRight, set, minDepth)
        } else {
          set.updated(sand, 'o')
        }
      }
    }
  }

  @tailrec
  def fillIt(input: Map[(Int, Int), Char], minDepth: Int, pouredAmount: Int): Int = {
    val startPoint     = (500, 0)
    val updatedSandSet = simulateOne(startPoint, input, minDepth)
    if (updatedSandSet.contains(startPoint)) {
      println("\n" + render(input))
      writeFile("result.txt", render(input))
      pouredAmount
    } else {
      fillIt(updatedSandSet, minDepth, pouredAmount + 1)
    }
  }

  override def run(inputFile: List[String]): String = {
    val rockSet  = parse(inputFile)
    val minDepth = rockSet.keys.map(_._2).max
    val count    = fillIt(rockSet, minDepth, 1)

    count.toString
  }
}
