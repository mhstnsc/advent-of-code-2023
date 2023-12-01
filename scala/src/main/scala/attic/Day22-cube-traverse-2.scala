package attic

import common._
import Day22_Problem1._

import scala.annotation.tailrec

object Day22_Problem2 extends MainBaseBig(22) {

  val Right = 0
  val Down  = 1
  val Left  = 2
  val Up    = 3

  val neighbours3dIncrements = List(
    Point3D(1, 0, 0),
    Point3D(-1, 0, 0),
    Point3D(0, 1, 0),
    Point3D(0, -1, 0),
    Point3D(0, 0, 1),
    Point3D(0, 0, -1)
  )

  def incrementToDirection(increment: Point): Int = {
    require(Math.abs(increment.l) + Math.abs(increment.c) == 1, increment)
    if (increment.c > 0) Right
    else if (increment.c < 0) Left
    else if (increment.l > 0) Down
    else Up
  }

  def computeCubeSize(map: Matrix[Char]): Int = {
    val nonSpaceCount = map.data.flatten.count(_ != ' ')

    (1 to 100).find(v => v * v == nonSpaceCount / 6).get
  }

  def findFacePoints(map: Matrix[Char], cubeSize: Int): Seq[Point] =
    for {
      l <- 0 until map.numLines() / cubeSize
      c <- 0 until map.numCols() / cubeSize
      p = Point(l * cubeSize, c * cubeSize)
      if map(p) != ' '
    } yield p

  // the cube is folded to the inside so to speak (or lateral faces coming to front)

  case class PlanePoints(
      planeVector: Point3D,
      contents: Map[Point3D, Point],
      foldRotations: List[Point3D]
  ) {
    require(Math.abs(planeVector.l) + Math.abs(planeVector.c) + Math.abs(planeVector.d) == 1)
  }

  /**
   * @param map
   * @return (folded cube, top left corner of the face used to found around
   */
  def foldCube(
      map: Matrix[Char]
  ): (Map[Point3D, PlanePoints], Point3D) = {
    val cubeSize    = computeCubeSize(map)
    val facePoints  = findFacePoints(map, cubeSize).map(p => Point3D(p.l, p.c, 0))
    val topLeftFace = facePoints.sortWith { case (a, b) => a.l < b.l || (a.l == b.l && a.c < b.c) }.head

    val unfoldedCube = facePoints.map { facePoint =>
      facePoint -> {
        val faceSlice =
          map.slice2D(Point(facePoint.l, facePoint.c), Point(facePoint.l + cubeSize, facePoint.c + cubeSize))
        PlanePoints(
          Point3D(0, 0, 1),
          faceSlice
            .values()
            .map { case (_, p) =>
              Point3D(p.l, p.c, 0).translate(facePoint) -> Point(p.l + facePoint.l, p.c + facePoint.c)
            }
            .toMap,
          Nil
        )

      }
    }.toMap

    (
      foldAroundFace(unfoldedCube, topLeftFace, cubeSize, Set.empty),
      topLeftFace
    )
  }

  def verifyCube(cube: Map[Point3D, PlanePoints]): Unit = {
    def projections(f: Iterable[Point3D]): (Matrix[Char], Matrix[Char], Matrix[Char]) = {
      val lc = f.map(v => Point(v.l, v.c))
      val dl = f.map(v => Point(v.d, v.l))
      val cd = f.map(v => Point(v.c, v.d))

      (
        Matrix.fromPoints(lc, '.', '*'),
        Matrix.fromPoints(dl, '.', '*'),
        Matrix.fromPoints(cd, '.', '*')
      )
    }

//    val firstCubeFace    = cube.keySet.find(_.d == 0).get
//    val allCubePoints    = cube.values.map(_.contents.keys).flatten
//    val shiftedCubeFaces = allCubePoints.map(p => p.translate(-firstCubeFace.l + 1, -firstCubeFace.c + 1, 0))

    cube.values.foreach { p =>
      val (a, b, c) = projections(p.contents.keys)
      println(s"Face ${p.planeVector}")
      println(a.mkString(""))
      println("---")
      println(b.mkString(""))
      println("---")
      println(c.mkString(""))
    }
  }

  private def foldAroundFace(
      faces: Map[Point3D, PlanePoints],
      face: Point3D,
      cubeSize: Int,
      visitedFaces: Set[Point3D]
  ): Map[Point3D, PlanePoints] =
    if (visitedFaces.contains(face)) {
      Map.empty
    } else {
      val newVisitedFaces = visitedFaces + face

      // if there is a face to the right then fold that and the result around l axis by 90 deg
      val faceRight = Option(face.copy(c = face.c + cubeSize)).filter(f => faces.contains(f))
      val faceLeft  = Option(face.copy(c = face.c - cubeSize)).filter(f => faces.contains(f))
      val faceDown  = Option(face.copy(l = face.l + cubeSize)).filter(f => faces.contains(f))

      val foldedRight = faceRight.map(f => foldAroundFace(faces, f, cubeSize, newVisitedFaces)).filter(_.nonEmpty)
      val foldedLeft  = faceLeft.map(f => foldAroundFace(faces, f, cubeSize, newVisitedFaces)).filter(_.nonEmpty)
      val foldedDown  = faceDown.map(f => foldAroundFace(faces, f, cubeSize, newVisitedFaces)).filter(_.nonEmpty)

      val rotatedRight = foldedRight.map(f =>
        f.map { case (p, PlanePoints(planeVector, cubePoints, foldRotations)) =>
          p ->
            PlanePoints(
              planeVector.rotateAroundPoint(Point3D.zero, dl = 1, dc = 0, dd = 0),
              cubePoints.map { case (p3d, p2d) =>
                p3d
                  .translate(0, 1, 0)
                  .rotateAroundPoint(face.translate(0, cubeSize, 0), dl = 1, dc = 0, dd = 0) -> p2d
              },
              Point3D(1, 0, 0) +: foldRotations
            )
        }
      )

      val rotatedLeft = foldedLeft.map(f =>
        f.map { case (p, PlanePoints(planeVector, cubePoints, foldRotations)) =>
          p ->
            PlanePoints(
              planeVector.rotateAroundPoint(Point3D.zero, dl = 3, dc = 0, dd = 0),
              cubePoints.map { case (p3d, p2d) =>
                p3d
                  .translate(0, -1, 0)
                  .rotateAroundPoint(face.translate(0, -1, 0), dl = 3, dc = 0, dd = 0) -> p2d
              },
              Point3D(3, 0, 0) +: foldRotations
            )

        }
      )

      val rotatedDown = foldedDown.map(f =>
        f.map { case (p, PlanePoints(planeVector, cubePoints, foldRotations)) =>
          p ->
            PlanePoints(
              planeVector.rotateAroundPoint(Point3D.zero, dl = 0, dc = 3, dd = 0),
              cubePoints.map { case (p3d, p2d) =>
                p3d
                  .translate(1, 0, 0)
                  .rotateAroundPoint(face.translate(cubeSize, 0, 0), dl = 0, dc = 3, dd = 0) -> p2d
              },
              Point3D(0, 3, 0) +: foldRotations
            )

        }
      )

      Map(
        face -> faces(face)
      ) ++ rotatedRight.getOrElse(Map.empty) ++ rotatedLeft.getOrElse(Map.empty) ++ rotatedDown.getOrElse(Map.empty)
    }

  override def run(inputFile: List[String]): String = {
    val (map, cmds) = parse(inputFile)

    val asciiRenderer = new AsciiRenderer(120, 80)

    val (foldedCube, topLeftFace) = foldCube(map)
    val allFoldedCubePoints       = foldedCube.values.map(_.contents).flatten.toMap

//    verifyCube(foldedCube)

    def findFace(pos: Point3D): (Point3D, PlanePoints) =
      foldedCube.find { case (_, PlanePoints(_, contents, _)) =>
        contents.contains(pos)
      }.get

    def unfoldVector(pos: Point3D, vector: Point3D): Point = {
      val (_, face) = findFace(pos)
      val unfoldedIncrement = face.foldRotations.foldLeft(vector) { case (acc, t) =>
        acc.rotateAroundPoint(Point3D.zero, t.l * -1, t.c * -1, t.d * -1)
      }
      require(unfoldedIncrement.d == 0, unfoldedIncrement)
      Point(unfoldedIncrement.l, unfoldedIncrement.c)
    }

    case class State private (
        map: Matrix[Char],
        increment: Point3D,
        pos: Point3D
    ) {
      def updateMapWithDebug(): State = {
        val unfoldedDirection = unfoldVector(pos, increment)

        val pos2D = allFoldedCubePoints(pos)
        this.copy(map = map.updated(pos2D, incrementToChar(unfoldedDirection)))
      }

      private def incrementToChar(increment: Point): Char =
        increment match {
          case Point(0, 0)  => 'X'
          case Point(1, 0)  => 'v'
          case Point(-1, 0) => '^'
          case Point(0, 1)  => '>'
          case Point(0, -1) => '<'
        }

      @tailrec
      final def move(amount: Int): State = {
        debugln(s"pos:${pos}, increment: ${increment}, face: ${findFace(pos)._1}")
        if (amount == 0) this
        else {
          val newPos = pos.translate(increment)
          val newState = if (!allFoldedCubePoints.contains(newPos)) {
            // we need to wrap it
            val neighbours3d = neighbours3dIncrements
              .map(incr => (incr, newPos.translate(incr)))
              .filter(p => allFoldedCubePoints.contains(p._2))
              .filter(p => p._2 != pos)

            require(neighbours3d.size == 1, s"${pos} -> ${neighbours3d}")
            this
              .copy(
                pos = neighbours3d.head._2,
                increment = neighbours3d.head._1
              )
              .updateMapWithDebug()
          } else
            this
              .copy(pos = newPos)
              .updateMapWithDebug()

          //          require(allFoldedCubePoints.contains(wrappedNewPos))

          if (map(allFoldedCubePoints(newState.pos)) == '#') {
            this
          } else {
            newState.move(amount - 1)
          }
        }
      }

      // rotating to the right is 27 degrees around the plane vector in trignometical way
      def incrementToRight(): State = {
        // determine the face vector
        val (_, PlanePoints(planeVector, _, _)) = findFace(pos)

        require(math.abs(planeVector.l) + math.abs(increment.l) <= 1)
        require(math.abs(planeVector.c) + math.abs(increment.c) <= 1)
        require(math.abs(planeVector.d) + math.abs(increment.d) <= 1)

        this
          .copy(increment =
            increment.rotateAroundPoint(Point3D.zero, planeVector.l * 3, planeVector.c * 3, planeVector.d * 3)
          )
          .updateMapWithDebug()
      }

      // rotating to the left is 90 degres around the plane vector in trigonometrical way
      def incrementToLeft(): State = {
        // determine the face vector
        val (_, PlanePoints(planeVector, _, _)) = findFace(pos)

        require(math.abs(planeVector.l) + math.abs(increment.l) <= 1)
        require(math.abs(planeVector.c) + math.abs(increment.c) <= 1)
        require(math.abs(planeVector.d) + math.abs(increment.d) <= 1)

        this
          .copy(increment = increment.rotateAroundPoint(Point3D.zero, planeVector.l, planeVector.c, planeVector.d))
          .updateMapWithDebug()
      }
    }

    val initialState = State(
      map,
      increment = Point3D(0, 1, 0),
      pos = topLeftFace
    ).updateMapWithDebug()

    val finalState = cmds.foldLeft(initialState) { case (acc, c) =>
      Thread.sleep(30)

      asciiRenderer.drawFrame(
        acc.map.mkString(
          "",
          renderer = (c: Char, _: Point) =>
            c match {
              case '<' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
              case '>' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
              case '^' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
              case 'v' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
              case '.' => s"${Terminal.ANSI_BLUE}$c${Terminal.ANSI_RESET}"
              case '#' => s"${Terminal.ANSI_BLUE}$c${Terminal.ANSI_RESET}"
              case _   => s"${Terminal.ANSI_BLUE}$c${Terminal.ANSI_RESET}"

            }
        ),
        allFoldedCubePoints(acc.pos)
      )
//      println(Terminal.moveCursorToPosition(1, 1))
//      println(
//        acc.map.mkString(
//          "",
//          renderer = (c: Char, p: Point) =>
//            c match {
//              case '<' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
//              case '>' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
//              case '^' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
//              case 'v' => s"${Terminal.ANSI_RED}$c${Terminal.ANSI_RESET}"
//              case '.' => s"${Terminal.ANSI_BLUE}$c${Terminal.ANSI_RESET}"
//              case '#' => s"${Terminal.ANSI_BLUE}$c${Terminal.ANSI_RESET}"
//              case _   => s"${Terminal.ANSI_BLUE}$c${Terminal.ANSI_RESET}"
//
//            }
//        )
//      )

      c match {
        case Move(amount) =>
          acc.move(amount)
        case RotateRight => acc.incrementToRight()
        case RotateLeft  => acc.incrementToLeft()
      }
    }
    debugln(finalState.pos + "" + finalState.increment)

    // to determine final orientation we have to revert the folding for the current increment
    val unfoldedIncrement = unfoldVector(finalState.pos, finalState.increment)
    val direction         = incrementToDirection(unfoldedIncrement)

    val finalPos2D = allFoldedCubePoints(finalState.pos)

    val result = (finalPos2D.l + 1) * 1000 + (finalPos2D.c + 1) * 4 + direction
    result.toString
  }
}
