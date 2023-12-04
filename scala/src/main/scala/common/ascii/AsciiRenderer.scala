package common.ascii

import common.{Point, Rectangle}

class AsciiRenderer(width: Int, height: Int) {

  private var lastPointOfInterest: Point = Point(height / 2, width / 2)
  private var region                     = Rectangle(Point(0, 0), Point(height, width))

  def drawFrame(frameContents: String, pointOfInterest: Point): Unit =
    drawFrame(frameContents.split("\n"), pointOfInterest)

  def drawFrame(frameContents: Seq[String], pointOfInterest: Point): Unit = {
    if (!region.contains(pointOfInterest)) {
      val increment = computeRegionTranslate(pointOfInterest)
      val newRegion = Rectangle(region.topLeft.translate(increment), region.bottomRight.translate(increment))
      region = newRegion
    }

    println(Terminal.moveCursorToPosition(1, 1))
    frameContents
      .slice(region.topLeft.l, region.bottomRight.l)
      .map(r => r.replaceAll("\u001B\\[\\d+m", ""))
      .map(row => row.slice(region.topLeft.c, region.bottomRight.c))
      .foreach(v => println(v))
  }

  private def computeRegionTranslate(point: Point): Point =
    ???
}
