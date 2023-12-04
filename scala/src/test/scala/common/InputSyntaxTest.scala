package common

import common.InputSyntax._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InputSyntaxTest extends AnyFunSuite with Matchers {
  test("split - basic") {
    List("a", "b", "sep", "c").splitVertically("sep") should contain theSameElementsInOrderAs (
      List(List("a", "b"), List("c"))
    )
    List("a", "b", "sep", "c", "sep").splitVertically("sep") should contain theSameElementsInOrderAs List(
      List("a", "b"),
      List("c")
    )
    List("sep", "a", "b", "sep", "c", "sep").splitVertically("sep") should contain theSameElementsInOrderAs List(
      List("a", "b"),
      List("c")
    )
  }
}
