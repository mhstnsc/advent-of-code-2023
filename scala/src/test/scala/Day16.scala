import attic.Day16_Problem1_Branch_Bound.Graph
import attic.Day16_Problem2_Branch_Bound.Branch
import attic.Day22_Problem2
import attic.Day22_Problem2.{findFacePoints, PlanePoints}
import common.{Matrix, Point, Point3D}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.TreeSet
class Day16 extends AnyFunSuite with Matchers {

  test("findFacePoints") {
    val g = Graph(
      arches = Map(
        "A"  -> Array("BB", "CC"),
        "BB" -> Array("A", "DD"),
        "CC" -> Array("A", "DD"),
        "DD" -> Array("CC", "BB")
      ),
      valves = Map(
        "A"  -> 0,
        "BB" -> 1,
        "CC" -> 1,
        "DD" -> 1
      )
    )

    g.transportCosts shouldBe Map(
      "BB" -> Map("A" -> 2, "CC" -> 3, "DD" -> 2, "BB" -> 1),
      "CC" -> Map("A" -> 2, "BB" -> 3, "DD" -> 2, "CC" -> 1),
      "DD" -> Map("BB" -> 2, "CC" -> 2, "A" -> 3, "DD" -> 1)
    )
  }

  test("branch - extrapolated") {
    Branch(
      Vector.empty,
      Vector.empty,
      elapsedTime = 1,
      rewardToTime = 1,
      increment = 100
    ).extrapolated(3) shouldBe Branch(
      Vector.empty,
      Vector.empty,
      elapsedTime = 3,
      rewardToTime = 201,
      increment = 100
    )
  }

  test("branch - appended") {
    Branch(
      Vector("a"),
      Vector(1),
      elapsedTime = 1,
      rewardToTime = 1,
      increment = 100
    ).appended("b", 3, 1000) shouldBe Branch(
      Vector("a", "b"),
      Vector(1, 4),
      elapsedTime = 4,
      rewardToTime = 301,
      increment = 1100
    )
  }

  test("producingValves") {

    val graph = Graph(
      arches = Map(
        "A"  -> Array("BB", "CC"),
        "BB" -> Array("A", "DD"),
        "CC" -> Array("A", "DD"),
        "DD" -> Array("CC", "BB")
      ),
      valves = Map(
        "A"  -> 4,
        "BB" -> 3,
        "CC" -> 2,
        "DD" -> 1
      )
    )

    val producingValves: TreeSet[String] = {
      implicit val o: Ordering[String] =
        (a: String, b: String) => graph.valves.getOrElse(b, 0) - graph.valves.getOrElse(a, 0)

      val values = graph.valves.keys.filter(v => graph.valves(v) > 0).toList
      TreeSet[String](values: _*)
    }

    producingValves.sliding(2, 2).map(v => (v.toIndexedSeq(0), v.toIndexedSeq(1))).toList shouldBe List(
      "A"  -> "BB",
      "CC" -> "DD"
    )

  }
}
