package common

case class Point(l: Int, c: Int) {
  override def toString: String =
    s"[$l,$c]"

  def manhattan(p: Point): Int =
    math.abs(p.l - l) + math.abs(p.c - c)

  def translate(p: Point) =
    Point(l + p.l, c + p.c)
}
