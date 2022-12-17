package common

import java.time.{Duration, Instant}
import scala.io.Source
import scala.util.Try

abstract class MainBase(day: Int) {

  def main(args: Array[String]): Unit = {
    Try(Source.fromResource(s"day-$day-test.txt").getLines().toList).toOption
      .orElse(
        Try(Source.fromResource(s"attic/day-$day-test.txt").getLines().toList).toOption
      )
      .foreach { smallInput =>
        println(s"test result = ${run(smallInput)}")
      }

    Try(Source.fromResource(s"day-$day-small.txt").getLines().toList).toOption
      .orElse(
        Try(Source.fromResource(s"attic/day-$day-small.txt").getLines().toList).toOption
      )
      .foreach { smallInput =>
        println(s"${getClass.getSimpleName} small result = ${run(smallInput)}")
      }

    val inputFile = Try(Source.fromResource(s"day-$day.txt").getLines().toList).toOption
      .orElse(
        Try(Source.fromResource(s"attic/day-$day.txt").getLines().toList).toOption
      )
      .get
    println(s"${getClass.getSimpleName} big   result = ${run(inputFile)}")
  }

  def run(inputFile: List[String]): String
}

abstract class MainBaseTest(day: Int) {

  def main(args: Array[String]): Unit =
    Try(Source.fromResource(s"day-$day-test.txt").getLines().toList).toOption
      .orElse(
        Try(Source.fromResource(s"attic/day-$day-test.txt").getLines().toList).toOption
      )
      .foreach { smallInput =>
        println(s"test result = ${run(smallInput)}")
      }

  def run(inputFile: List[String]): String
}

abstract class MainBaseSmall(day: Int) {

  def main(args: Array[String]): Unit = {
    Try(Source.fromResource(s"day-$day-small.txt").getLines().toList).toOption
      .orElse(
        Try(Source.fromResource(s"attic/day-$day-small.txt").getLines().toList).toOption
      )
      .foreach { smallInput =>
        println(s"${getClass.getSimpleName} small result = ${run(smallInput)}")
      }
  }

  def run(inputFile: List[String]): String
}

abstract class MainBaseBig(day: Int) {

  def main(args: Array[String]): Unit = {
    val inputFile = Try(Source.fromResource(s"day-$day.txt").getLines().toList).toOption
      .orElse(
        Try(Source.fromResource(s"attic/day-$day.txt").getLines().toList).toOption
      )
      .get
    val startTime = Instant.now()
    val v         = run(inputFile)
    val endTime   = Instant.now()
    println(s"${getClass.getSimpleName} big   result [${Duration.between(endTime, startTime).toSeconds}] = ${v}")
  }

  def run(inputFile: List[String]): String
}
