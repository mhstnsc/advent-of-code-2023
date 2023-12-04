package common

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
class MatrixTest extends AnyFunSuite with Matchers {

  val fixture = new Matrix[Int](
    Vector(
      Vector(1, 2, 3, 4),
      Vector(5, 6, 7, 8),
      Vector(9, 10, 11, 12)
    )
  )

  test("sizes") {
    fixture.numCols() shouldBe 4
    fixture.numLines() shouldBe 3
  }
  test("transpose") {
    fixture.transpose() shouldBe new Matrix(
      Vector(
        Vector(1, 5, 9),
        Vector(2, 6, 10),
        Vector(3, 7, 11),
        Vector(4, 8, 12)
      )
    )
  }
  test("rotate") {
    fixture.rotateClockwise(2) shouldBe Matrix(
      Vector(
        Vector(12, 11, 10, 9),
        Vector(8, 7, 6, 5),
        Vector(4, 3, 2, 1)
      )
    )
  }

  test("grow") {
    fixture.grow(0) shouldBe Matrix(
      Vector(
        Vector(0, 0, 0, 0, 0, 0),
        Vector(0, 1, 2, 3, 4, 0),
        Vector(0, 5, 6, 7, 8, 0),
        Vector(0, 9, 10, 11, 12, 0),
        Vector(0, 0, 0, 0, 0, 0)
      )
    )
  }

  test("updated") {
    fixture.updated(1, 1, 100)(1, 1) shouldBe 100
  }

  test("updatedLine") {
    fixture.updatedLine(0, Vector(50, 60, 70, 90))(0) shouldBe Vector(50, 60, 70, 90)
  }

  test("updatedCol") {
    fixture.updatedCol(0, Vector(60, 70, 90)).column(0) shouldBe Vector(60, 70, 90)
  }

  test("slice1D") {
    fixture.slice1D(2, 3, -1, -1) shouldBe Vector(12, 7, 2)
    fixture.slice1D(1, 1, 1, 1) shouldBe Vector(6, 11)
  }

  test("mapLines") {
    fixture.mapLines { (line, lineIdx) =>
      line.map(v => v + lineIdx + 1)
    } shouldBe Matrix(
      Vector(
        Vector(2, 3, 4, 5),
        Vector(7, 8, 9, 10),
        Vector(12, 13, 14, 15)
      )
    )
  }

  test("mapColumns") {
    fixture.mapColumns { (line, lineIdx) =>
      line.map(v => v + lineIdx + 1)
    } shouldBe Matrix(
      Vector(
        Vector(2, 4, 6, 8),
        Vector(6, 8, 10, 12),
        Vector(10, 12, 14, 16)
      )
    )
  }

  test("print") {
    println(fixture.mkString(" "))
  }
}
