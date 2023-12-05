import common.InputSyntax.InputSyntax
import common.{Interval, MainBase}

case class Mapping(source: Interval, destination: Interval)

case class Input(seeds: Vector[Long], maps: Vector[Vector[Mapping]])

abstract class Day5Base extends MainBase(5) {
  implicit val ordering: Ordering[Mapping] = (x: Mapping, y: Mapping) =>
    Interval.orderingRange.compare(x.source, y.source)

  private def parseSection(section: List[String]): Vector[Mapping] =
    section
      .drop(1)
      .map { s =>
        val serializedMaping = s.split(" ").map(_.toLong)
        Mapping(
          source = Interval(serializedMaping(1), serializedMaping(2)),
          destination = Interval(serializedMaping(0), serializedMaping(2))
        )
      }
      .toVector

  def parseInput(inputFile: List[String]): Input = {
    val sections = inputFile.splitVertically("")
    val seeds    = sections(0).head.split("[: ]").drop(2).map(_.toLong).toVector
    val maps     = sections.drop(1).map(parseSection).toVector.map(_.sorted)
    Input(seeds, maps)
  }
}

private object Day5Problem1 extends Day5Base {
  override def run(inputFile: List[String]): String = {
    val input = parseInput(inputFile)
    input.seeds.map(cascadingSearch(input)).min.toString
  }

  private def searchMapping(map: Vector[Mapping], v: Long): Long = {
    def searchImpl(startIdx: Int, endIdx: Int): Long =
      if (startIdx < endIdx) {
        val mid     = (endIdx + startIdx) / 2
        val mapping = map(mid)
        if (mapping.source.start <= v && v < mapping.source.end) {
          val r = mapping.destination.start + (v - mapping.source.start)
          r
        } else if (v < mapping.source.start) {
          searchImpl(startIdx, mid)
        } else {
          searchImpl(mid + 1, endIdx)
        }
      } else {
        v
      }
    searchImpl(0, map.length)
  }

  def cascadingSearch(input: Input)(seed: Long): Long =
    input.maps.foldLeft(seed) { case (v, map) =>
      searchMapping(map, v)
    }
}

object Day5Problem2 extends Day5Base {

  def intersect(a: Interval, b: Interval): Option[Interval] =
    if (a.start <= b.start) {
      if (a.end <= b.start) {
        None
      } else {
        Some(Interval(b.start, Math.min(a.end - b.start, b.length)))
      }
    } else {
      intersect(b, a)
    }

  def substract(a: Interval, b: Interval): Vector[Interval] = {
    val common = intersect(a, b)
    common
      .map(c => Vector(Interval(a.start, c.start - a.start), Interval(c.end, a.end - c.end)).filter(_.length > 0))
      .getOrElse(Vector(a))
  }

  def union(a: Interval, b: Interval): Vector[Interval] = {
    val common = intersect(a, b)
    common.map { c =>
      Vector(Interval(Math.min(a.start, b.start), a.length + b.length - c.length))
    }.getOrElse(Vector(a, b).sorted)
  }

  def union(r: Vector[Interval]): Vector[Interval] =
    r.sorted.foldLeft(Vector.empty[Interval]) { case (acc, v) =>
      acc.dropRight(1) ++ acc.lastOption.map(last => union(last, v)).getOrElse(Vector(v))
    }

  private def searchMapping(map: Vector[Mapping], range: Interval): Vector[Interval] = {
    val mapped = map.map { m =>
      val common = intersect(m.source, range)
      common.map { c =>
        Interval(m.destination.start + (c.start - m.source.start), c.length)
      }
    }.flatten

    val verbatim = map.foldLeft(Vector(range)) { case (acc, mapping) =>
      acc.map { a =>
        substract(a, mapping.source)
      }.flatten
    }

    union(mapped ++ verbatim)
  }

  private def cascadingMap(input: Input)(range: Interval): List[Interval] =
    input.maps.foldLeft(List(range)) { case (ranges, map) =>
      val r                  = ranges.map(v => searchMapping(map, v)).flatten
      r
    }

  override def run(inputFile: List[String]): String = {
    val input = parseInput(inputFile)
    val inputRanges = input.seeds
      .sliding(2, 2)
      .map(range => Interval(range(0), range(1)))
      .toVector
    inputRanges.map(r => cascadingMap(input)(r)).flatten.min.start.toString
  }
}
