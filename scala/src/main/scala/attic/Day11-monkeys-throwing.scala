package attic

import attic.Day11.{executeRounds, parse}
import common.InputSyntax.InputSyntax
import common.MainBase

import scala.math.BigDecimal.RoundingMode

object Day11 {

  def parse(inputFile: List[String]): Vector[MonkeyConfig] =
    inputFile
      .splitVertically("")
      .map { monkeyLines =>
        val myIndex = monkeyLines(0).split(":")(0).split(" ")(1).toInt
        val items = monkeyLines(1)
          .split(":")(1)
          .split(",")
          .map(v => BigDecimal(v.trim))
          .toVector
        val operation = {
          val signalAndOp = monkeyLines(2).split(" ").takeRight(2)
          signalAndOp(0) match {
            case "+" =>
              val operand = if (signalAndOp(1) == "old") None else Some(BigDecimal(signalAndOp(1).trim.toLong))
              ((x: BigDecimal) => x + operand.getOrElse(x), s"+ $operand")
            case "*" =>
              val operand = if (signalAndOp(1) == "old") None else Some(BigDecimal(signalAndOp(1).trim))
              ((x: BigDecimal) => x * operand.getOrElse(x), s"* $operand")
            case _ => throw new Exception("unknownop")
          }
        }
        val divisibleBy = monkeyLines(3).split(" ").last.toInt
        val throwTrue   = monkeyLines(4).split(" ").last.toInt
        val throwFalse  = monkeyLines(5).split(" ").last.toInt

        MonkeyConfig(myIndex, items, operation._1, operation._2, divisibleBy, throwTrue, throwFalse, 0)
      }
      .toVector

  case class MoveCommand(
      newValue: BigDecimal,
      targetMonkeyIndex: Int
  )

  case class MonkeyConfig(
      myIndex: Int,
      items: Vector[BigDecimal],
      operation: BigDecimal => BigDecimal,
      operationDebug: String,
      divisibleBy: Int,
      throwTrue: Int,
      throwFalse: Int,
      processCount: BigDecimal
  ) {
    def computeTarget(item: BigDecimal): MoveCommand = {
      val newVal = operation(item)
      if (newVal % divisibleBy == 0) MoveCommand(newVal, throwTrue)
      else MoveCommand(newVal, throwFalse)
    }

    def processItems(): (MonkeyConfig, Seq[MoveCommand]) = {
      val newTargets = items.map(computeTarget)

      val myItems = (items zip newTargets).filter { case (_, moveCommand) =>
        moveCommand.targetMonkeyIndex == myIndex
      }.map(_._1)

      val toThrowItems = (items zip newTargets).filter { case (_, MoveCommand(_, newMonkeyIndex)) =>
        newMonkeyIndex != myIndex
      }.map { case (_, MoveCommand(newItem, newMonkeyIndex)) =>
        MoveCommand(newItem, newMonkeyIndex)
      }

      (
        this.copy(items = myItems, processCount = this.processCount + items.size),
        toThrowItems
      )
    }
  }

  def printObjects(configs: Vector[MonkeyConfig]) =
    configs.foreach { m =>
      println(s"m[${m.myIndex}]=${m.items.mkString(" ")} /  ${m.processCount}")
    }

  def throwFor(monkeyIndex: Int, old: Vector[MonkeyConfig]): Vector[MonkeyConfig] = {
    val (myEntry, destinationMonkeys) = old(monkeyIndex).processItems()
    val updatedMonkeys = destinationMonkeys
      .foldLeft(old) { case (acc, MoveCommand(item, i)) =>
        acc.updated(i, acc(i).copy(items = acc(i).items.appended(item)))
      }
      .updated(monkeyIndex, myEntry)
    updatedMonkeys
  }

  def executeRounds(value: Int, monkeyConfigs: Vector[MonkeyConfig]): BigDecimal = {
    val resulting = (0 until value).foldLeft(monkeyConfigs) { (acc, _) =>
      acc.indices.foldLeft(acc) { case (acc, i) =>
        val newConfig = throwFor(i, acc)
        newConfig
      }
    }
    val topMonkeys = resulting.sortBy(_.processCount)(Ordering.BigDecimal.reverse).take(2)
    topMonkeys(0).processCount * topMonkeys(1).processCount
  }
}

object Day111Problem1 extends MainBase(11) {

  override def run(inputFile: List[String]): String = {
    val monkeyConfigs = parse(inputFile)
    val dividedMonkeyConfigs =
      monkeyConfigs.map { v =>
        v.copy(operation = (o: BigDecimal) => (v.operation(o) / 3).setScale(0, RoundingMode.FLOOR))
      }
    executeRounds(20, dividedMonkeyConfigs).toString()
  }
}

object Day11Problem2 extends MainBase(11) {
  override def run(inputFile: List[String]): String = {
    val monkeyConfigs = parse(inputFile)
    val commonDivisor = monkeyConfigs.map(_.divisibleBy).foldLeft(1L) { case (acc, v) => acc * v }

    // this is the trick of the problem.
    // To keep numbers within bounds we modulo a common multiplier for all divisors
    // we alter the operations to incorporate this one
    val moduloMonkeyConfigs =
      monkeyConfigs.map { v =>
        v.copy(operation = (o: BigDecimal) => v.operation(o) % commonDivisor)
      }
    executeRounds(10000, moduloMonkeyConfigs).toString()
  }
}
