package common

// d from depth or z
// l from lines or y-axis
// c from column or x-axis
case class Point3D(l: Int, c: Int, d: Int) {
  override def toString: String =
    s"[$l,$c,$d]"

  // rotate trigonometrical around the axes where
  // - C to the right
  // - L to down
  // - D going doen left(or towards viewer
  def rotateOneAxis(dl: Int, dc: Int, dd: Int): Point3D ={
    if (dl != 0) {
      val rotatedPoint = rotateOneAxis(Point(c, d), dl)
      Point3D(l, rotatedPoint.l, rotatedPoint.c)
    } else if (dc != 0) {
      val rotatedPoint = rotateOneAxis(Point(d, l), dc)
      Point3D(rotatedPoint.c, c, rotatedPoint.l)
    } else if (dd != 0) {
      val rotatedPoint = rotateOneAxis(Point(l, c), dd)
      Point3D(rotatedPoint.l, rotatedPoint.c, d)
    } else this
  }


  // rotates into trigonometrical way around the axis(right hand way)
  // the l axis is Y going down
  // the c axis is X going right
  private def rotateOneAxis(point: Point, amount: Int): Point =
    if (amount == 0) point
    else if (amount == 1 || amount == -3) Point(-point.c, point.l)
    else if (amount == 2 || amount == -2) Point(-point.l, -point.c)
    else if (amount == 3 || amount == -1) Point(point.c, -point.l)
    else throw new Exception(s"amount not supported ${amount}")

  def translate(dl: Int, dc: Int, dd: Int): Point3D =
    Point3D(l + dl, c + dc, d + dd)
  def translate(increment: Point3D): Point3D =
    Point3D(l + increment.l, c + increment.c, d + increment.d)

  def rotateAroundPoint(center: Point3D, dl: Int, dc: Int, dd: Int): Point3D =
    translate(-center.l, -center.c, -center.d)
      .rotateOneAxis(dl, dc, dd)
      .translate(center.l, center.c, center.d)
}

object Point3D {
  val zero = Point3D(0,0,0)
}