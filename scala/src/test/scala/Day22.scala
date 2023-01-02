import attic.Day22_Problem2
import attic.Day22_Problem2.{PlanePoints, findFacePoints}
import common.{Matrix, Point, Point3D}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
class Day22 extends AnyFunSuite with Matchers {

  test("findFacePoints") {
    val map = Matrix[Char](
      Vector(
        "  ..  ".toVector,
        "  ..  ".toVector,
        "......".toVector,
        "......".toVector,
        "  ..  ".toVector,
        "  ..  ".toVector,
        "  ..  ".toVector,
        "  ..  ".toVector
      )
    )
    Day22_Problem2.findFacePoints(map, 2) should contain theSameElementsAs (
      Seq(
        Point(0, 2),
        Point(2, 0),
        Point(2, 2),
        Point(2, 4),
        Point(4, 2),
        Point(6, 2)
      )
    )
  }

  test("foldCube") {
    val map = Matrix[Char](
      Vector(
        " . ".toVector,
        "...".toVector,
        " . ".toVector,
        " . ".toVector
      )
    )

    val cube       = Day22_Problem2.foldCube(map)
    val cubePoints = cube._1.map(_._2.contents.keySet).flatten

    // check the rotated 3d points
    cubePoints should contain theSameElementsAs List(
      Point3D(0, 1, 0),
      Point3D(0, 1, 2),
      Point3D(1, 1, 1),
      Point3D(0, 2, 1),
      Point3D(0, 0, 1),
      Point3D(-1, 1, 1)
    )

    // check the 3d to 2d mappings
    val points2D = cube._1.map(_._2.contents.values).flatten
    points2D should contain theSameElementsAs (
      findFacePoints(map, 1)
    )

    // check the face keys
    cube._1.keySet.map(v => Point(v.l + v.d, v.c + v.d)) should contain theSameElementsAs findFacePoints(map, 1)

    cube._1(Point3D(3, 1, 0)).foldRotations should contain theSameElementsInOrderAs (
      List(
        Point3D(0, 3, 0),
        Point3D(0, 3, 0),
        Point3D(0, 3, 0)
      )
    )

    cube shouldBe (
      Map(
        Point3D(0, 1, 0) -> PlanePoints(Point3D(0, 0, 1), Map(Point3D(0, 1, 0) -> Point(0, 1)), Nil)
      )
    )
  }

  test("foldAnotherCube") {
    val map = Matrix[Char](
      Vector(
        "  . ".toVector,
        "... ".toVector,
        "  ..".toVector
      )
    )

    val cube       = Day22_Problem2.foldCube(map)
    val cubePoints = cube._1.map(_._2.contents.keySet.map(_.translate(0, -1, 0))).flatten

    // check the rotated 3d points
    cubePoints should contain theSameElementsAs List(
      Point3D(0, 1, 0),
      Point3D(0, 1, 2),
      Point3D(1, 1, 1),
      Point3D(0, 2, 1),
      Point3D(0, 0, 1),
      Point3D(-1, 1, 1)
    )

    // check the 3d to 2d mappings
    val points2D = cube._1.map(_._2.contents.values).flatten
    points2D should contain theSameElementsAs (
      findFacePoints(map, 1)
    )

    // check the face keys
    cube._1.keySet.map(v => Point(v.l + v.d, v.c + v.d)) should contain theSameElementsAs findFacePoints(map, 1)

  }

}
