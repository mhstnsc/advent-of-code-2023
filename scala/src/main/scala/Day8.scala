import Day8.ProblemBase
import common.MainBase

object Day8 {
  class ProblemBase extends MainBase(8) {
    type Matrix = Vector[Vector[Int]]
    def parse(inputLines: List[String]): Matrix =
      inputLines.map(s => s.map(_.toInt - '0').toVector).toVector

    override def run(inputFile: List[String]): String = {
      val matrix   = parse(inputFile)
      val numCols  = matrix(0).size
      val numLines = matrix.size

      def isVisibleFromOutside(slice: Seq[Int], value: Int): Boolean =
        !slice.exists(v => v >= value)

      val visibilities = for {
        l <- 1 until matrix.size - 1
        c <- 1 until matrix(0).size - 1
      } yield {
        val beforeLine = matrix(l).slice(0, c).reverse
        val afterLine  = matrix(l).slice(c + 1, numCols)
        val beforeCol  = { for { i <- 0 until l } yield matrix(i)(c) }.reverse.toVector
        val afterCol   = { for { i <- l + 1 until numLines } yield matrix(i)(c) }.toVector
        val value      = matrix(l)(c)

        isVisibleFromOutside(beforeLine, value) || isVisibleFromOutside(afterLine, value) ||
        isVisibleFromOutside(beforeCol, value) || isVisibleFromOutside(afterCol, value)
      }

      val insideVisibility  = visibilities.count(v => v)
      val outsideVisibility = (2 * matrix(0).size) + 2 * (matrix.size - 2)

      (insideVisibility + outsideVisibility).toString
    }
  }
}

object Day8Problem1 extends ProblemBase

object Day8Problem2 extends ProblemBase {

  override def run(inputFile: List[String]): String = {
    val matrix = parse(inputFile)

    println(visibleDistance(Vector(1, 2, 3), 0))

    def visibleDistance(slice: Vector[Int], value: Int): Int =
      slice.zipWithIndex.find { case (v, _) =>
        v >= value
      }.map(_._2 + 1).getOrElse(slice.size)

    def score(l: Int, c: Int): Int = {
      val numCols    = matrix(0).size
      val numLines   = matrix.size
      val beforeLine = matrix(l).slice(0, c).reverse
      val afterLine  = matrix(l).slice(c + 1, numCols)
      val beforeCol  = { for { i <- 0 until l } yield matrix(i)(c) }.reverse.toVector
      val afterCol   = { for { i <- l + 1 until numLines } yield matrix(i)(c) }.toVector

      val value = matrix(l)(c)

      visibleDistance(beforeLine, value) *
        visibleDistance(afterLine, value) *
        visibleDistance(beforeCol, value) *
        visibleDistance(afterCol, value)
    }

    val scores = for {
      l <- 1 until matrix.size - 1
      c <- 1 until matrix(0).size - 1
    } yield score(l, c)

    scores.max.toString
  }

}
