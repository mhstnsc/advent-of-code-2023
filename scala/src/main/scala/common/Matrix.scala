package common

case class Point(l: Int, c: Int)

case class Matrix[T](data: Vector[Vector[T]]) {

  def numLines() = data.size
  def numCols()  = Option.when(data.nonEmpty)(data(0).size).getOrElse(0)

  def col(c: Int): Vector[T] = {
    for { i <- 0 until numLines() } yield data(i)(c)
  }.toVector

  def slice(l: Int, c: Int, stepL: Int, stepC: Int): Vector[T] =
    if (l < 0 || l >= numLines() || c < 0 || c >= numCols()) {
      Vector.empty
    } else {
      slice(l + stepL, c + stepC, stepL, stepC).prepended(data(l)(c))
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

  def rotateClockwise(degree: Int): Matrix[T] = {
    def rotateOne(): Matrix[T] =
      Matrix(
        {
          for { ci <- 0 until numCols() } yield data.map(v => v(ci)).reverse
        }.toVector
      )

    if (degree == 0) {
      this
    } else {
      rotateOne().rotateClockwise(degree - 1)
    }
  }

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
