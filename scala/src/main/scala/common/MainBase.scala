package common

import scala.io.Source

abstract class MainBase(day: Int) {
  def main(args: Array[String]): Unit = {
    val inputFile = Source.fromResource(s"day-$day.txt").getLines().toList
    run(inputFile)
  }

  def run(inputFile: List[String])
}