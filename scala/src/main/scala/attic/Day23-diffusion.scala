package attic

import attic.Day23_Problem1.{GlobalState, computeArea, oneRound, parse}
import common._

object Day23_Problem1 extends MainBaseBig(23) {

  val North = 0
  val South = 1
  val West  = 2
  val East  = 3

  val increments =
    Vector(
      (-1 to 1).map(c => Point(-1, c)),
      (-1 to 1).map(c => Point(1, c)),
      (-1 to 1).map(c => Point(c, -1)),
      (-1 to 1).map(c => Point(c, 1))
    )
  val incrementsAround = increments.flatten.toSet

  case class ElfState(
      p: Point,
      lastMoved: Option[Int]
  )

  def parse(inputFile: List[String]): Vector[ElfState] =
    inputFile.zipWithIndex.map { case (line, l) =>
      line.zipWithIndex.map { case (value, c) =>
        if (value == '#') {
          Some(ElfState(Point(l, c), None))
        } else {
          None
        }
      }.flatten.toVector
    }.flatten.toVector

  case class GlobalState(
      elves: Map[Point, ElfState]
  )

  def computeArea(globalState: GlobalState): Int = {
    val minLine = globalState.elves.keySet.map(_.l).min
    val maxLine = globalState.elves.keySet.map(_.l).max
    val minCol  = globalState.elves.keySet.map(_.c).min
    val maxCol  = globalState.elves.keySet.map(_.c).max

    (maxLine - minLine + 1) * (maxCol - minCol + 1) - globalState.elves.size
  }

  def printElves(startState: GlobalState, finalState: GlobalState): Unit = {
    val minLine = startState.elves.keySet.map(_.l).min
    val maxLine = startState.elves.keySet.map(_.l).max
    val minCol  = startState.elves.keySet.map(_.c).min
    val maxCol  = startState.elves.keySet.map(_.c).max

    val finalMinLine = finalState.elves.keySet.map(_.l).min
    val finalMaxLine = finalState.elves.keySet.map(_.l).max
    val finalMinCol  = finalState.elves.keySet.map(_.c).min
    val finalMaxCol  = finalState.elves.keySet.map(_.c).max

    val m = Matrix.filled(maxLine - minLine + 1, maxCol - minCol + 1, '.')
    val result = startState.elves.keySet.foldLeft(m) { case (acc, m) =>
      acc.updated(m.l - minLine, m.c - minCol, '#')
    }
    println(result.mkString(""))
    println("\n")
  }

  def oneRound(globalState: GlobalState, currentOrientation: Int): GlobalState = {
    case class FoldState(
        current: Map[Point, ElfState],
        prevPos: Map[Point, Point],
        collisions: Set[Point]
    )
    val initialState = FoldState(Map.empty, Map.empty, Set.empty)

    val finalState = globalState.elves.foldLeft(initialState) { case (acc, (elfPos, elfState)) =>
      val shouldMove = incrementsAround.map(inc => Point(elfPos.l + inc.l, elfPos.c + inc.c))
        .exists(p => globalState.elves.contains(p))

      val allowedMoves =
        Option.when(shouldMove)(()).flatMap { _ =>
          val candidateMoves = (0 to 3)
            .map(i => (currentOrientation + i) % 4)
            .map(orientation => (orientation, increments(orientation)))
            .map { case (orientation, moves) =>
              (orientation, moves.map(move => Point(elfPos.l + move.l, elfPos.c + move.c)))
            }
          candidateMoves.find { case (_, candidatePoss) =>
            candidatePoss.forall(candidatePos => !globalState.elves.contains(candidatePos))
          }
            .map { case (orientation, nextMove) => (orientation, nextMove)}
        }

      val candidateFoldState = allowedMoves.map { case (orientation, moves) => (orientation, moves(1)) }.map {
        case (orientation, move) =>
          if (acc.current.contains(move)) {
            // there's a collision, so do not move the elf
            acc.copy(
              current = acc.current + (elfPos -> elfState.copy(lastMoved = None)),
              collisions = acc.collisions + move
            )
          } else {
            // elf can move freely
            FoldState(
              current = acc.current + (move -> ElfState(move, Some(orientation))),
              prevPos = acc.prevPos + (move -> elfPos),
              collisions = acc.collisions
            )
          }
      }
        .getOrElse(
          // elf did not need to move
          acc.copy(current = acc.current + (elfPos -> elfState.copy(lastMoved = None)))
        )
      candidateFoldState
    }
    // we process collisions and roll back possitions
    val collisionsRemoved = FoldState(
      current = finalState.collisions.foldLeft(finalState.current) { case (acc, collision) =>
        acc - collision + (finalState
          .prevPos(collision) -> ElfState(finalState.prevPos(collision), None))
      },
      prevPos = Map.empty,
      collisions = Set.empty
    )

    require(collisionsRemoved.current.size == globalState.elves.size)
    val r = GlobalState(
      collisionsRemoved.current
    )
//    printElves(globalState, r)
    r
  }

  override def run(inputFile: List[String]): String = {
    val elves = parse(inputFile)

    val result = (0 until 10).foldLeft(GlobalState(elves.map(v => v.p -> v).toMap)) { case (acc, i) =>
      oneRound(acc, i % 4)
    }

    computeArea(result).toString
  }
}


object Day23_Problem2 extends MainBaseBig(23) {
  override def run(inputFile: List[String]): String = {
    val elves = parse(inputFile)

    val result = (0 until 1000).foldLeft(GlobalState(elves.map(v => v.p -> v).toMap)) { case (acc, i) =>
      val updatedState = oneRound(acc, i % 4)
      if(updatedState.elves == acc.elves) {
        throw new Exception(s"answer${i}")
      }
      updatedState
    }

    computeArea(result).toString
  }
}