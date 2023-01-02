package attic

import common._

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.util.Random
import Day16_Problem1_Branch_Bound._

object Day16_Problem1_DP extends MainBaseBig(16) {

  val maxTime = 30

  case class AccValue(
      reward: Int,
      increment: Int,
      intermediateValves: Set[String],
      chainOfValves: Vector[String]
  ) {}

  case class AccKey(
      startValve: String,
      endValve: String,
      time: Int
  )

  override def run(inputFile: List[String]): String = {
    val graph = parse(inputFile)

    val maxTime = 30

    val producingValves =
      Random.shuffle(graph.valves.keys.filter(v => graph.valves(v) > 0).toSet.toIndexedSeq)

    val initialAcc: Map[AccKey, AccValue] = {
      for {
        i    <- producingValves.prepended("AA")
        j    <- producingValves.prepended("AA")
        time <- 0 to maxTime
        if time >= graph.transportCosts(i)(j)
      } yield AccKey(i, j, time) -> AccValue(graph.transportCosts(i)(j), graph.valves(j), Set(j), Vector(j))
    }.toMap

    def getPath(accKey: AccKey, values: Map[AccKey, AccValue]): Option[AccValue] =
      values.get(accKey).orElse(getDirectPath(accKey))

    def getDirectPath(accKey: AccKey): Option[AccValue] = {
      val transportCost = graph.transportCosts(accKey.startValve)(accKey.endValve)
      Option.when(transportCost <= accKey.time) {
        AccValue(
          reward = graph.valves(accKey.endValve) * (accKey.time - transportCost),
          increment = graph.valves(accKey.endValve),
          intermediateValves = Set(accKey.endValve),
          chainOfValves = Vector(accKey.endValve)
        )
      }
    }

    def solve(producingValves: IndexedSeq[String]): Map[AccKey, AccValue] = {
      val finalState = {
        for {
          k        <- 0 until producingValves.size
          i        <- 0 until producingValves.size
          j        <- 0 until producingValves.size
          iToKTime <- 0 to maxTime
          kToJTime <- 0 to maxTime - iToKTime
        } yield (i, k, j, iToKTime, kToJTime)
      }.foldLeft(initialAcc) { case (acc, (i, k, j, iToKTime, kToJTime)) =>
        val pathKey = AccKey(producingValves(i), producingValves(j), iToKTime + kToJTime)

        val newPathO =
          for {
            iToK <- getPath(AccKey(producingValves(i), producingValves(k), iToKTime), acc)
            //            kToJ <- getDirectPath(AccKey(producingValves(k), producingValves(j), kToJTime))
            kToJ <- getPath(AccKey(producingValves(k), producingValves(j), kToJTime), acc)
            if iToK.intermediateValves.intersect(kToJ.intermediateValves).isEmpty
          } yield AccValue(
            reward = iToK.reward + (iToK.increment * kToJTime) + kToJ.reward,
            increment = iToK.increment + kToJ.increment,
            intermediateValves = iToK.intermediateValves ++ kToJ.intermediateValves,
            chainOfValves = iToK.chainOfValves ++ kToJ.chainOfValves
          )

        {
          for {
            newPath <- newPathO
            currentPath = getPath(pathKey, acc)
            if currentPath.isEmpty || currentPath.exists(_.reward < newPath.reward)
          } yield acc.updated(pathKey, newPath)
        }.getOrElse(acc)
      }
      finalState
    }

    val lastTimes = {
      val valves   = producingValves.prepended("AA")
      val solution = solve(valves)
      for {
        time <- 1 to maxTime
        j    <- graph.valves.keys
        path <- solution.get(AccKey("AA", j, time))
        extrapolatedReward = path.reward + path.increment * (maxTime - time)
      } yield (extrapolatedReward, path)
    }.maxBy(v => v._1)

    println(lastTimes)

    lastTimes.toString
  }

  def print(timeIndex: Int, graph: Graph, acc: Map[AccKey, AccValue]): Unit = {
    println(s"Time budget: ${timeIndex}")
    val printMatrix = {
      for {
        i <- graph.valves.keys.toList.sorted.zipWithIndex
        j <- graph.valves.keys.toList.sorted.zipWithIndex
      } yield (i, j)
    }.foldLeft(Matrix.filled(graph.valves.size, graph.valves.size, "")) { case (m, (i, j)) =>
      m.updated(
        i._2,
        j._2,
        acc
          .get(AccKey(i._1, j._1, timeIndex))
          .map(v => (v.increment, v.chainOfValves.mkString(",")).toString())
          .getOrElse("X")
      )
    }
    println(printMatrix.mkString(" "))
    println("")
  }
}
