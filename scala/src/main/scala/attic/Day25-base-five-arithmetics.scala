package attic

import common._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

object Day25_Problem1 extends MainBaseBig(25) {

  def parse(inputFile: List[String]) = {}

  def snafuToDecimalDigit(c: Char): Int =
    c match {
      case '=' => -2
      case '-' => -1
      case '0' => 0
      case '1' => 1
      case '2' => 2
    }

  def decimalToSnafuDigit(c: Int): Char =
    c match {
      case -2 => '='
      case -1 => '-'
      case 0  => '0'
      case 1  => '1'
      case 2  => '2'
    }

  def pow5(i: Int): BigInt =
    if (i == 0) 1 else 5 * pow5(i - 1)

  def snafuToDecimal(s: String): BigInt =
    s.reverse.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (c, i)) =>
      acc + pow5(i) * snafuToDecimalDigit(c)
    }

  case class Solution(
      digits: Vector[Int],
      value: BigInt
  ) {
    def appendDigit(snafuDigit: Int): Solution = {
      val pwr = digits.size
      Solution(
        digits.appended(snafuDigit),
        value = value + pow5(pwr) * snafuDigit
      )
    }

    def toSnafuNumber(): String =
      digits.map(decimalDigit => decimalToSnafuDigit(decimalDigit)).reverse.mkString("")
  }

  def decimalToSnafu(decimalTarget: BigInt): String = {
    @tailrec
    def explore(
        explorationQueue: mutable.PriorityQueue[Solution]
    ): Option[Solution] =
      if (explorationQueue.isEmpty) {
        None
      } else {
        val current = explorationQueue.head
        if (current.value > decimalTarget || current.digits.size > 10) {
          explore(explorationQueue.drop(1))
        } else if (current.value == decimalTarget) {
          Some(current)
        } else {
          val incremented = (-2 to 2).map(snafuDigit => current.appendDigit(snafuDigit))
          explore(
            explorationQueue.drop(1) ++ incremented
          )
        }
      }

    implicit val ordering: Ordering[Solution] = (x: Solution, y: Solution) =>
      if (y.value > x.value) -1 else if (y.value == x.value) 0 else 1
    val pqueue = new mutable.PriorityQueue[Solution]()

    val maybeSolution = explore(pqueue.addOne(Solution(Vector.empty, 0)))
    maybeSolution.get.toSnafuNumber()
  }

  def decimalToSnafuV2(decimalTarget: BigInt): String = {
    case class Memoization(
        s: Map[Int, Vector[Solution]]
    )

    val maxMemoization = 8
    def exploreWithMemoization(
        order: Int,
        memoization: Memoization
    ): Memoization = {
      println(s"exploreWithMemoization(${order}, $maxMemoization}")
      if (order > maxMemoization) {
        memoization
      } else {
        val memoizationData = for {
          k        <- (1 until order).par
          sk       <- memoization.s(k)
          sjminusk <- memoization.s(order - k)
        } yield {
          val candidate = sk.value + 5 * sjminusk.value
          val s = Solution(
            value = candidate,
            digits = sk.digits ++ sjminusk.digits
          )
          if (candidate == decimalTarget) {
            throw new Exception(s.toSnafuNumber())
          } else {
            s
          }
        }
        exploreWithMemoization(
          order + 1,
          memoization.copy(s = memoization.s + (order -> memoizationData.toVector))
        )
      }
    }

    val initialSolutiions  = (-2 to 2).map(v => Solution(Vector(v), v)).toVector
    val initialMemoization = Memoization(Map(1 -> initialSolutiions))

    val memoization = exploreWithMemoization(2, initialMemoization)

    def exploreRecursively(
        value: BigInt,
        order: Int
    ): Unit =
      if (order > maxMemoization) {
        for {
          k <- 1 until maxMemoization
        }
          if (k < maxMemoization) {
            for {
              sk <- memoization.s(k)
            }
              exploreRecursively(value + 5 * sk.value, order - k)
          }
      } else {
        for {
          sk <- memoization.s(order)
        }
          if (value + 5 * sk.value == decimalTarget) {
            throw new Exception(sk.toSnafuNumber())
          }
      }

    (maxMemoization to Int.MaxValue).foreach { v =>
      exploreRecursively(0, v)
    }
    ""
  }

  def sum(a: String, b: String): String = {
    def digitSum(a: Char, b: Char, carry: Int): (Char, Int) = {
      val digitSum = snafuToDecimalDigit(a) + snafuToDecimalDigit(b) + carry

      val newCarry = digitSum / 5
      val rest     = digitSum % 5
      val (safeCarry, safeRest) = if (rest > 2) {
        (newCarry + 1, rest - 5)
      } else if (rest < -2) {
        (newCarry - 1, rest + 5)
      } else
        (newCarry, rest)

      (decimalToSnafuDigit(safeRest), safeCarry)
    }

    if (a.length > b.length) {
      sum(b, a)
    } else {
      val (commonPartSum, commonPartCarry) = a.reverse
        .padTo(b.length, '0')
        .zip(b.reverse)
        .foldLeft[(List[Char], Int)]((Nil, 0)) { case ((acc, carry), (a, b)) =>
          val (sumChar, newCarry) = digitSum(a, b, carry)
          (sumChar +: acc, newCarry)
        }

      val r = if(commonPartCarry != 0) {
        decimalToSnafuDigit(commonPartCarry) +: commonPartSum
      } else {
        commonPartSum
      }

      r.mkString("")
    }

  }

  override def run(inputFile: List[String]): String = {
    val s = inputFile.map(snafuToDecimal).sum

    println(s"decimal ${sum("10", "1=")}")


    val sumResult = inputFile.foldLeft("0") {
      case (acc, v) => sum(acc, v)
    }

//    println(s"decimal sum ${s}")

    sumResult
  }

}
