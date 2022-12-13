package attic

import attic.Day12.{ExploreCandidate, bfs, buildPath, parse}
import common.{MainBase, Matrix, Point}

import scala.annotation.tailrec
import scala.util.Try

object Day12 {

  def parse(inputFile: List[String]): Matrix[Char] =
    Matrix(inputFile.map(_.toVector).toVector)

  def canMove(matrix: Matrix[Char], point: Point, point1: Point): Boolean = {
    val elevationStart = if (matrix(point.l, point.c) == 'S') 'a' else matrix(point.l, point.c)
    val elevationEnd   = if (matrix(point1.l, point1.c) == 'E') 'z' else matrix(point1.l, point1.c)

    elevationEnd.toInt - elevationStart.toInt <= 1
  }

  case class HistoryItem(parent: Point)
  case class ExploreCandidate(candidate: Point, parent: Point)

  @tailrec
  def bfs(
      matrix: Matrix[Char],
      explorationQueue: Vector[ExploreCandidate],
      explorationSet: Set[Point],
      end: Point,
      history: Map[Point, HistoryItem]
  ): Map[Point, HistoryItem] =
    if (explorationQueue.isEmpty) {
      throw new Exception("We did not reach destination")
    } else {
      val ExploreCandidate(current, parent) = explorationQueue.head
      val updatedHistory                    = history.updated(current, HistoryItem(parent))

      if (current == end) {
        updatedHistory
      } else {
        val top   = current.copy(l = current.l - 1)
        val down  = current.copy(l = current.l + 1)
        val left  = current.copy(c = current.c - 1)
        val right = current.copy(c = current.c + 1)

        val newNodes = List(top, down, left, right)
          .filter(v => matrix.isInside(v))
          .filter(v => !updatedHistory.contains(v))
          .filter(p => canMove(matrix, current, p))
          .filter(p => !explorationSet.contains(p))
          .map(v => ExploreCandidate(v, current))

        val updatedExplorationQueue = (explorationQueue ++ newNodes.toVector).drop(1)
        val updatedExplorationSet   = explorationSet ++ newNodes.map(_.candidate)

        bfs(matrix, updatedExplorationQueue, updatedExplorationSet, end, updatedHistory)
      }
    }

  def buildPath(history: Map[Point, HistoryItem], start: Point, end: Point): Vector[Point] =
    if (start == end) {
      Vector(start)
    } else {
      buildPath(history, start, history(end).parent).appended(end)
    }
}

object Day12Problem1 extends MainBase(12) {

  override def run(inputFile: List[String]): String = {
    val matrix     = parse(inputFile)
    val startPoint = matrix.find(_ == 'S').head
    val endPoint   = matrix.find(_ == 'E').head

    val initialExplorationQueue = Vector(ExploreCandidate(startPoint, startPoint))
    val history                 = bfs(matrix, initialExplorationQueue, Set.empty, endPoint, Map.empty)

    val path = buildPath(history, startPoint, endPoint)
    path.size.toString
  }
}

object Day12Problem2 extends MainBase(12) {
  override def run(inputFile: List[String]): String = {
    val matrix = parse(inputFile)

    val startPoints = matrix.find(_ == 'a')
    val endPoint    = matrix.find(_ == 'E').head

    startPoints.flatMap { p =>
      val initialExplorationQueue = Vector(ExploreCandidate(p, p))
      Try(bfs(matrix, initialExplorationQueue, Set.empty, endPoint, Map.empty)).toOption
        .map(history => buildPath(history, p, endPoint).size)
    }.min.toString
  }
}
