package common

import scala.io.Source
import scala.util.Try

abstract class MainBase(day: Int) {
  def main(args: Array[String]): Unit = {
    Try(Source.fromResource(s"day-$day-test.txt").getLines().toList).toOption.foreach { smallInput =>
      println(s"test result = ${run(smallInput)}")
    }

    Try(Source.fromResource(s"day-$day-small.txt").getLines().toList).toOption.foreach { smallInput =>
      println(s"small result = ${run(smallInput)}")
    }

    val inputFile = Source.fromResource(s"day-$day.txt").getLines().toList
    println(s"result = ${run(inputFile)}")
  }

  def run(inputFile: List[String]): String
}
