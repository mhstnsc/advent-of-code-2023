package common

import scala.collection.immutable.Range

case class Interval(start: Long, length: Long) {
  def end: Long         = start + length
  def isEmpty: Boolean  = length == 0
  def nonEmpty: Boolean = length > 0

  require(length >= 0)

  override def toString: String = s"[${start}, $end)"
}

object Interval {
  implicit val orderingRange: Ordering[Interval] = (x: Interval, y: Interval) =>
    if (x.end - 1L < y.start)
      -1
    else 1
}

object RangeSyntax {
  implicit class RangeOps(a: Interval) {
    def intersect(b: Interval): Option[Interval] =
      if (a.start <= b.start) {
        if (a.end <= b.start) {
          None
        } else {
          Some(Interval(b.start, Math.min(a.end - b.start, b.length)))
        }
      } else {
        b.intersect(a)
      }

    def substract(b: Interval): Vector[Interval] = {
      val common = a.intersect(b)
      common
        .map(c => Vector(Interval(a.start, c.start - a.start), Interval(c.end, a.end - c.end)).filter(_.length > 0))
        .getOrElse(Vector(a))
    }

    def union(b: Interval): Vector[Interval] = {
      val common = a.intersect(b)
      common.map { c =>
        Vector(Interval(Math.min(a.start, b.start), a.length + b.length - c.length))
      }.getOrElse(Vector(a, b).sorted)
    }

    def union(r: Vector[Interval]): Vector[Interval] =
      r.sorted.foldLeft(Vector.empty[Interval]) { case (acc, v) =>
        acc.dropRight(1) ++ acc.lastOption.map(_.union(v)).getOrElse(Vector(v))
      }
  }
}
