package common

/**
 * Swiss army knife Matrix
 *
 * Features:
 * map() - items
 * mapLines
 * mapColumns
 *
 * slice1D - slice in any direction and step
 * slice2D - slice a submatrix with any bounds -> check updatedMatrix to patch
 *
 * transpose
 * rotate
 * grow (to grow around)
 *
 * multiply
 * sum
 *
 * updated(item)
 * updated(submatrix)
 * updatedCol
 * updatedLine
 */
case class Matrix[T](data: Vector[Vector[T]]) {

  def numLines() = data.size
  def numCols()  = Option.when(data.nonEmpty)(data(0).size).getOrElse(0)

  def isInside(p: Point): Boolean =
    p.l >= 0 && p.l < numLines() && p.c >= 0 && p.c < numCols()

  def column(c: Int): Vector[T] = {
    for { i <- 0 until numLines() } yield data(i)(c)
  }.toVector

  def map(f: (T, Int, Int) => T): Matrix[T] =
    Matrix(
      data.zipWithIndex.map { case (line, lineIdx) =>
        line.zipWithIndex.map { case (v, colIdx) =>
          f(v, lineIdx, colIdx)
        }
      }
    )

  def values(): Iterable[(T, Point)] =
    data.zipWithIndex.map { case (line, lineIdx) =>
      line.zipWithIndex.map { case (v, colIdx) =>
        (v, Point(lineIdx, colIdx))
      }
    }.flatten

  def find(f: T => Boolean): Vector[Point] =
    data.zipWithIndex.flatMap { case (line, lineIdx) =>
      line.zipWithIndex.flatMap { case (v, colIdx) =>
        val r = if (f(v)) Some(Point(lineIdx, colIdx)) else None
        r
      }
    }

  def mapLines(f: Vector[T] => Vector[T]): Matrix[T] =
    mapLines((line, _) => f(line))

  def mapLines(f: (Vector[T], Int) => Vector[T]): Matrix[T] =
    Matrix(data.zipWithIndex.map { case (column, colIdx) => f(column, colIdx) })

  def mapColumns(f: Vector[T] => Vector[T]): Matrix[T] =
    transpose().mapLines(f).transpose()

  def mapColumns(f: (Vector[T], Int) => Vector[T]): Matrix[T] =
    transpose().mapLines(f).transpose()

  def slice1D(l: Int, c: Int, stepL: Int, stepC: Int): Vector[T] =
    if (l < 0 || l >= numLines() || c < 0 || c >= numCols()) {
      Vector.empty
    } else {
      slice1D(l + stepL, c + stepC, stepL, stepC).prepended(data(l)(c))
    }

  def slice2D(from: Point, until: Point): Matrix[T] = {
    Matrix(
      data.slice(from.l, until.l).map(
        l => l.slice(from.c, until.c)
      )
    )
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

  def mul(matrix: Matrix[T], f: (T, T) => T): Matrix[T] = ???
  def sum(matrix: Matrix[T], f: (T, T) => T): Matrix[T] = ???

  def updated(l: Int, c: Int, value: T): Matrix[T] = new Matrix(data.updated(l, data(l).updated(c, value)))
  def updated(p: Point, value: T): Matrix[T]       = updated(p.l, p.c, value)
  def updated(from: Point, matrix: Matrix[T]): Matrix[T] = {
    val until = Point(
      from.l + matrix.numLines(),
      from.c + matrix.numCols()
    )
    require(from.l + matrix.numLines() <= numLines(), s"from: ${from} ${matrix.numLines()} ${numLines()}")
    require(from.c + matrix.numCols() <= numCols())
    Matrix(
      data.slice(0, from.l) ++
        data.slice(from.l, until.l).zip(matrix.data).map { case (oldLine, newLine) =>
          oldLine.slice(0, from.c) ++ newLine ++ oldLine.slice(until.c, Int.MaxValue)
        } ++
        data.slice(until.l, Int.MaxValue)
    )
  }

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

  def mkString(
      sep: String,
      renderer: (T, Point) => String = (v, _) => v.toString,
      alignColumns: Boolean = true
  ): String = {
    // find column widths
    val colWidths = transpose().data.map { col =>
      col.map(v => v.toString.length).maxOption.getOrElse(0)
    }
    data.zipWithIndex.map { case (line, lineIdx) =>
      line.zipWithIndex.map { case (v, colIdx) =>
        val colWidth   = colWidths(colIdx)
        val prefixSize = if (alignColumns) colWidth - v.toString.length else 0
        " " * prefixSize + renderer(v, Point(lineIdx, colIdx))
      }.mkString(sep)
    }.mkString("\n")
  }

  override def toString: String = mkString(" ")

}

object Matrix {
  implicit class IndexingOps[T](matrix: Matrix[T]) {
    def apply(l: Int, c: Int): T = matrix.data(l)(c)
    def apply(l: Int): Vector[T] = matrix.data(l)
    def apply(p: Point): T       = matrix.data(p.l)(p.c)
  }

  def filled[T](numLines: Int, numCols: Int, default: T): Matrix[T] =
    new Matrix(
      Vector.fill(numLines)(Vector.fill(numCols)(default))
    )

  def fromPoints[T](points: Iterable[Point], emptyValue: T, filledValue: T): Matrix[T] = {
    val minL = points.map(_.l).min
    val minC = points.map(_.c).min
    val numLines = points.map(_.l).max - minL + 1
    val numCols = points.map(_.c).max - minC + 1

    points.foldLeft(Matrix.filled(numLines, numCols, emptyValue)) {
      case (acc, p) => acc.updated(p.l - minL, p.c - minC, filledValue)
    }
  }

}
