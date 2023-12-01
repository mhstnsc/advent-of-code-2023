package common

case class Rectangle(topLeft: Point, bottomRight: Point) {
  def contains(p: Point): Boolean =
    p.l >= topLeft.l && p.l < bottomRight.l && p.c >= topLeft.c && p.c < bottomRight.c
}
