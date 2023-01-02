package attic

import common.InputSyntax.InputSyntax
import common._

import scala.annotation.tailrec

object Day22_Problem1 extends MainBaseBig(22) {

  sealed trait Command
  case class Move(amount: Int) extends Command
  case object RotateRight            extends Command
  case object RotateLeft             extends Command

  def parse(inputFile: List[String]) = {
    val sections = inputFile.splitVertically("")

    val numCols  = sections(0).map(_.length).max

    val matrix = Matrix(sections(0).map(l => l + " " * (numCols - l.length)).map(_.toVector).toVector)

    def parseCmds(remaining: String, acc: List[Command]): List[Command] =
      if (remaining.isEmpty) {
        acc
      } else {
        if (remaining(0) == 'R') {
          parseCmds(remaining.drop(1), acc :+ RotateRight)
        } else if (remaining(0) == 'L') {
          parseCmds(remaining.drop(1), acc :+ RotateLeft)
        } else {
          val rotationIndex = remaining.indexWhere(c => c == 'R' || c == 'L')
          val safeIndex = if (rotationIndex == -1) {
            remaining.length
          } else {
            rotationIndex
          }
          parseCmds(remaining.drop(safeIndex), acc :+ Move(remaining.slice(0, safeIndex).toInt))
        }
      }

    (matrix, parseCmds(sections(1).head, Nil))
  }

  def cubeSize(map: Matrix[Char]): Int = {
    val nonSpaceCount = map.data.flatten.count(_ != ' ')

    (0 to 20).find(v => v * v == nonSpaceCount / 6).get
  }

  def facePoints(map: Matrix[Char], cubeSize: Int): Seq[Point] =
    for {
      l <- 0 to map.numLines() / cubeSize
      c <- 0 to map.numCols() / cubeSize
      p = Point(l * cubeSize, c * cubeSize)
      if map(p) != ' '
    } yield p

  case class FaceBorder(
      p: Point,
      orientation: Int
  )

  def allFaceBorders(facePoints: Seq[Point]): Seq[FaceBorder] =
    for {
      p            <- facePoints
      orientatiion <- 0 to 3
    } yield FaceBorder(p, orientatiion)

  val Right = 0
  val Down  = 1
  val Left  = 2
  val Up    = 3

  def providedFaceConnections(facePoints: Seq[Point], map: Matrix[Char], cubeSize: Int): Map[FaceBorder, FaceBorder] = {
    for {
      l <- 0 to map.numLines() / cubeSize
      c <- 0 to map.numCols() / cubeSize
      p = Point(l * cubeSize, c * cubeSize)
      if facePoints.contains(p)
      downPoint  = Some(p.copy(l = p.l + cubeSize)).find(p => map.isInside(p))
      rightPoint = Some(p.copy(c = p.c + cubeSize)).find(p => map.isInside(p))
    } yield IndexedSeq(
      downPoint.map(d =>
        IndexedSeq(FaceBorder(p, Down) -> FaceBorder(d, Up), FaceBorder(d, Up) -> FaceBorder(p, Down))
      ),
      rightPoint.map(d =>
        IndexedSeq(FaceBorder(p, Right) -> FaceBorder(d, Left), FaceBorder(d, Left) -> FaceBorder(p, Right))
      )
    ).flatten.flatten
  }.flatten.toMap

  def determineCornerConnections(connections: Map[FaceBorder, FaceBorder], facePoints: Seq[Point]): Map[FaceBorder, FaceBorder] = {
    {
      for {
        p           <- facePoints
        orientation <- 0 to 3
        cornerOrientation = (orientation + 1) % 3
        connection       <- connections.get(FaceBorder(p, orientation))
        cornerConnection <- connections.get(FaceBorder(p, cornerOrientation))
      } yield Seq(
        FaceBorder(connection.p, cornerOrientation) -> FaceBorder(cornerConnection.p, orientation),
        FaceBorder(cornerConnection.p, orientation) -> FaceBorder(connection.p, cornerOrientation)
      )
    }.flatten.toMap
  }

//  def determineCircles(connections: Map[FaceBorder, FaceBorder], facePoints: Seq[Point]): Map[FaceBorder, FaceBorder] = {
//    val missingConnections = allFaceBorders(facePoints).toSet.diff(connections.keySet)
//    for {
//      combination <- facePoints.combinations(4)
//      startDirection <- 0 to 3
//      if combination.toSet.size == 4  // all unique faces
//
//    } yield ???
//
//  }

  override def run(inputFile: List[String]): String = {
    val (map, cmds) = parse(inputFile)
    case class State(
        map: Matrix[Char],
        orientation: Int,
        pos: Point
    )

    val startColumn = map.data.head.indexWhere(c => c == '.')
    require(startColumn != -1)

    val initialState = State(
      map,
      orientation = 0,
      pos = Point(0, startColumn)
    )

    val increments = Vector(
      Point(0, 1),
      Point(1, 0),
      Point(0, -1),
      Point(-1, 0)
    )

    val perLineBorders: Map[Int, (Point, Point)] = map.data.zipWithIndex.map { case (l, idx) =>
      val leftColumn  = l.indexWhere(c => c != ' ')
      val rightColumn = l.lastIndexWhere(c => c != ' ')
      idx -> (Point(idx, leftColumn), Point(idx, rightColumn))
    }.toMap

    val perColumnBorders: Map[Int, (Point, Point)] =
      (0 until map.numCols()).map { colIdx =>
        val c          = map.column(colIdx)
        val topLine    = c.indexWhere(c => c != ' ')
        val bottomLine = c.lastIndexWhere(c => c != ' ')
        colIdx -> (Point(topLine, colIdx), Point(bottomLine, colIdx))
      }.toMap

    val finalState = cmds.foldLeft(initialState) { case (acc, c) =>
      def wrapLine(point: Point): Point = {
        val perLineBorder = perLineBorders(point.l)
        if (point.c > perLineBorder._2.c) {
          perLineBorder._1
        } else if (point.c < perLineBorder._1.c) {
          perLineBorder._2
        } else point
      }

      def wrapColumn(point: Point): Point = {
        val perColumnBorder = perColumnBorders(point.c)
        if (point.l > perColumnBorder._2.l) {
          perColumnBorder._1
        } else if (point.l < perColumnBorder._1.l) {
          perColumnBorder._2
        } else point
      }

      @tailrec
      def move(amount: Int, point: Point, direction: Int): Point =
        if (amount == 0) point
        else {
          val increment     = increments(direction)
          val newPos        = Point(point.l + increment.l, point.c + increment.c)
          val wrappedNewPos = if (direction == 0 || direction == 2) wrapLine(newPos) else wrapColumn(newPos)
          require(map.isInside(wrappedNewPos))

          if (map(wrappedNewPos) == '#') {
            point
          } else {
            move(amount - 1, wrappedNewPos, direction)
          }
        }

      def directionToChar(direction: Int): Char = {
        val chars = Vector('>', 'v', '<', '^')
        chars(direction)
      }
//      println(acc.pos)
//      println(acc.map.mkString(""))
      c match {
        case Move(amount) =>
          val newPosition = move(amount, acc.pos, acc.orientation)
          acc.copy(pos = newPosition, map = acc.map.updated(acc.pos.l, acc.pos.c, directionToChar(acc.orientation)))
        case RotateRight => acc.copy(orientation = (acc.orientation + 1) % 4)
        case RotateLeft  => acc.copy(orientation = if (acc.orientation - 1 == -1) 3 else acc.orientation - 1)
      }
    }
    println(finalState.pos)

    val result = (finalState.pos.l + 1) * 1000 + (finalState.pos.c + 1) * 4 + finalState.orientation
    result.toString
  }
}
