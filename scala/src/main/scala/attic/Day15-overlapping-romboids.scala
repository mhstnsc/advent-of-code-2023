package attic

import attic.Day15._
import common._

import scala.util.Random

object Day15 {

  def parse(input: List[String]): List[(Point, Point)] = {
    val re = "Sensor at x=([0-9\\-]+), y=([0-9\\-]+): closest beacon is at x=([0-9\\-]+), y=([0-9\\-]+)".r
    input.map {
      case re(sensorX, sensorY, beaconX, beaconY) =>
        (Point(sensorY.toInt, sensorX.toInt), Point(beaconY.toInt, beaconX.toInt))
      case _ => throw new Exception("unknown format")
    }
  }

  def distance(a: Point, b: Point): Int = math.abs(a.l - b.l) + math.abs(a.c - b.c)

  def computeSensorBounds(map: List[(Point, Point)]): Map[Int, List[(Point, Point)]] =
    map.flatMap { case (sensor, beacon) =>
      val radius = distance(sensor, beacon)
      (sensor.l - radius - 1 to sensor.l + radius + 1).map(l => (l, (sensor, beacon)))
    }.groupBy { case (line, _) => line }.view.mapValues(l => l.map(_._2)).toMap

  def computeNoBeaconPos(line: Int, sensorBounds: Map[Int, List[(Point, Point)]]): Set[Point] = {
    val intersectingPairs = sensorBounds.getOrElse(line, Nil)
    val expandedIntersections = intersectingPairs.flatMap { case (sensor, beacon) =>
      val radius          = distance(sensor, beacon)
      val candidatePoints = (sensor.c - radius - 1 to sensor.c + radius + 1).map(v => Point(line, v))
      val intersectingPoints = candidatePoints.filter { c =>
        distance(sensor, c) <= radius
      }
      intersectingPoints
    }.toSet
    expandedIntersections
  }

  def computeForRow(line: Int, map: List[(Point, Point)]): Int = {
    val beaconsOnTheLine      = map.filter { case (_, beacon) => beacon.l == line }.map(_._2).toSet
    val sensorsOnTheLine      = map.filter { case (sensor, _) => sensor.l == line }.map(_._1).toSet
    val sensorBounds          = computeSensorBounds(map)
    val expandedIntersections = computeNoBeaconPos(line, sensorBounds)

    expandedIntersections.diff(beaconsOnTheLine).diff(sensorsOnTheLine).size
  }

  def perimeter(sensor: Point, beacon: Point): Seq[Point] = {
    val radius      = distance(sensor, beacon) + 1
    val columnDelta = (0 to radius) ++ (0 until radius).reverse
    (sensor.l - radius to sensor.l + radius).zip(columnDelta).flatMap { case (line, columnDelta) =>
      Seq(
        Point(line, sensor.c - columnDelta),
        Point(line, sensor.c + columnDelta)
      )
    }
  }

  def surface(sensor: Point, beacon: Point): Seq[Point] = {
    val radius      = distance(sensor, beacon)
    val columnDelta = (0 to radius) ++ (0 until radius).reverse
    (sensor.l - radius to sensor.l + radius).zip(columnDelta).flatMap { case (line, columnDelta) =>
      (sensor.c - columnDelta to sensor.c + columnDelta).map(c => Point(line, c))
    }
  }

  def isInsideRegion(regionSize: Int)(point: Point): Boolean =
    (point.l >= 0 & point.l <= regionSize) && point.c >= 0 && point.c <= regionSize

  def isInsideSensor(point: Point)(sensor: Point, beacon: Point): Boolean = {
    val radius = distance(sensor, beacon)
    distance(sensor, point) <= radius
  }

  def computePointForMatrix(bound: Int, map: List[(Point, Point)]): BigInt = {
    val pointsInsideRegion = map.flatMap { case (sensor, beacon) =>
      perimeter(sensor, beacon).filter(isInsideRegion(bound))
    }.filter(p => !map.exists { case (sensor, beacon) => isInsideSensor(p)(sensor, beacon) }).distinct

    pointsInsideRegion.head.c
  }

  def computeForMatrix(bound: Int, map: List[(Point, Point)]): BigInt = {
    val pointsInsideRegion = map.flatMap { case (sensor, beacon) =>
      perimeter(sensor, beacon).filter(isInsideRegion(bound))
    }.filter(p => !map.exists { case (sensor, beacon) => isInsideSensor(p)(sensor, beacon) }).distinct

    require(pointsInsideRegion.size == 1, s"$pointsInsideRegion")
    println(pointsInsideRegion)
    BigInt(pointsInsideRegion.head.c) * 4000000 + BigInt(pointsInsideRegion.head.l)
  }
}

object Day15Problem1Small extends MainBaseSmall(15) {
  override def run(inputFile: List[String]): String = {
    val pairs = parse(inputFile)

    computeForRow(10, pairs).toString

  }
}

object Day15Problem1 extends MainBase(15) {
  override def run(inputFile: List[String]): String = {
    val pairs = parse(inputFile)

    computeForRow(2000000, pairs).toString
  }
}

object Day15Problem2Small extends MainBaseSmall(15) {
  override def run(inputFile: List[String]): String = {
    val pairs = parse(inputFile)

    val m = pairs.map { case (sensor, beacon) =>
      surface(sensor, beacon).filter(isInsideRegion(20))
    }.zipWithIndex.foldLeft(Matrix.filled(21, 21, -1)) {
      case (acc, (v, _)) =>
        val seed = Random.nextInt(Int.MaxValue)
        v.foldLeft(acc){ case (acc, point: Point) => acc.updated(point.l, point.c, seed)}
    }

    val rendered = m.mkString("", renderer = (v, _)=> if(v == -1) s"-" else s"${Terminal.getRandom256Color(v)}o${Terminal.ANSI_RESET}", alignColumns = false)
    FileUtils.writeFile("result.txt", rendered)
    println(rendered)

    computeForMatrix(20, pairs).toString
  }
}

object Day15Problem2 extends MainBaseBig(15) {
  override def run(inputFile: List[String]): String = {
    val pairs = parse(inputFile)
    computeForMatrix(4000000, pairs).toString
  }
}
