package common
import scala.annotation.unused

case class Point(l: Int, c: Int)

class Matrix[T](val data: Vector[Vector[T]]) {

  def numLines() = data.size
  def numCols()  = Option.when(data.nonEmpty)(data(0).size).getOrElse(0)

  def sliceLineTo(l: Int, c: Int): Vector[T]    = data(l).slice(0, c)
  def sliceLineAfter(l: Int, c: Int): Vector[T] = data(l).slice(c + 1, numCols())
  def sliceColBefore(l: Int, c: Int): Vector[T] = { for { i <- 0 until l } yield data(i)(c) }.toVector
  def sliceColAfter(l: Int, c: Int): Vector[T]  = { for { i <- l + 1 until numLines } yield data(i)(c) }.toVector

  def sliceUntil(point1: Point, point2: Point): Matrix[T] = {
    val minLine = point1.l min point2.l
    val maxLine = point1.l max point2.l
    val minCol  = point1.c min point1.c
    val maxCol  = point1.c max point1.c

    new Matrix((for { i <- minLine until maxLine } yield data(i).slice(minCol, maxCol)).toVector)
  }

  def transpose(): Matrix[T] =
    new Matrix({
      for {
        c <- 0 until numCols()
      } yield {
        for {
          l <- 0 until numLines()
        } yield data(l)(c)
      }.toVector
    }.toVector)

  def updated(l: Int, c: Int, value: T): Matrix[T] = new Matrix(data.updated(l, data(l).updated(c, value)))
  def updatedLine(l: Int, newLine: Vector[T]): Matrix[T] = {
    require(newLine.size == numCols())
    new Matrix(
      data.updated(l, newLine)
    )
  }
  def updatedCol(c: Int, newCol: Vector[T]): Matrix[T] = {
    require(newCol.size == numLines())
    new Matrix(
      (data zip newCol).map { case (line, value) =>
        line.updated(c, value)
      }
    )
  }
  def grow(defaultValue: T): Matrix[T] =
    new Matrix({
      val newNumCols  = numCols() + 2
      val newLine     = Vector.from((0 until newNumCols).map(_ => defaultValue))
      val moreColumns = data.map(l => l.prepended(defaultValue).appended(defaultValue))
      moreColumns.prepended(newLine).appended(newLine)
    })
}

object Matrix {
  implicit class IndexingOps[T](matrix: Matrix[T]) {
    def apply(l: Int, c: Int): T = matrix.data(l)(c)
    def apply(l: Int): Vector[T] = matrix.data(l)
  }
}
