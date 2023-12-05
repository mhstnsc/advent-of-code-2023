import common.Interval
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5Test extends AnyFlatSpec with Matchers {
  it should "substract" in {
    Day5Problem2.substract(Interval(0, 10), Interval(1, 2)) shouldBe Vector(Interval(0, 1), Interval(3, 7))
    Day5Problem2.substract(Interval(0, 10), Interval(11, 2)) shouldBe Vector(Interval(0, 10))
    Day5Problem2.substract(Interval(0, 10), Interval(-1, 2)) shouldBe Vector(Interval(1, 9))
    Day5Problem2.substract(Interval(0, 10), Interval(9, 2)) shouldBe Vector(Interval(0, 9))
  }

  it should "union" in {
    Day5Problem2.union(Interval(0, 10), Interval(1, 2)) shouldBe Vector(Interval(0, 10))
    Day5Problem2.union(Interval(0, 10), Interval(11, 2)) shouldBe Vector(Interval(0, 10), Interval(11, 2))
    Day5Problem2.union(Interval(0, 10), Interval(-1, 2)) shouldBe Vector(Interval(-1, 11))
  }

  it should "union all" in {
    Day5Problem2.union(Vector(Interval(0, 10), Interval(11, 2), Interval(-1, 2))) shouldBe Vector(Interval(-1, 11), Interval(11, 2))
  }
}
