import Day16_2._
import common._

import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec
import scala.collection.parallel.mutable.ParArray

object Day16_2 {
  val day = 16

  case class Graph(
      arches: Map[String, Array[String]],
      valves: Map[String, Int],
      transportCosts: Map[String, Map[String, Int]] // for each node we have a map to a destination node with a cost
  ) {
    val producingValves: Array[String] = valves.keys.filter(k => valves(k) > 0).toVector.prepended("AA").sorted.toArray
  }

  def parse(input: List[String]): Graph =
    input.map { l =>
      val arr    = l.split(";")
      val selfRe = "Valve (\\w+) has flow rate=(\\d+)".r
      val thisNode = arr(0) match {
        case selfRe(thisValve, thisValue) => (thisValve, thisValue.toInt)
      }
      val siblings =
        arr(1).replace(" tunnels lead to valves ", "").replace(" tunnel leads to valve ", "").split(",").map(_.trim)

      (thisNode._1 -> thisNode._2, thisNode._1 -> siblings)
    }
      .foldLeft(Graph(Map.empty, Map.empty, Map.empty)) { case (acc, (valveSpec, childrenSpec)) =>
        Graph(arches = acc.arches + childrenSpec, acc.valves + valveSpec, Map.empty)
      }

  case class ExpandResult(
      travelCost: Int,
      accumulatedReward: Int
  )

  @tailrec
  def expand(
      graph: Graph,
      opened: Set[String],
      maxTime: Int,
      elapsed: Int,
      explorationQueue: Vector[(String, Int)],
      accumulator: Map[String, ExpandResult],
      maxDepth: Int,
      visited: Set[String]
  ): Map[String, ExpandResult] =
    if (explorationQueue.isEmpty || maxDepth == 1)
      accumulator
    else {
      val (currentValue, cost) = explorationQueue.head

      val siblings = graph
        .arches(currentValue)
        .filter(v => !visited.contains(v))
        .filter(v => !explorationQueue.contains(v))
        .map(v => (v, cost + 1))

      val newAccumulator =
        if (opened.contains(currentValue)) accumulator
        else
          accumulator + (currentValue -> ExpandResult(
            cost + 1,
            graph.valves(currentValue) * (maxTime - elapsed - cost - 1)
          ))
//      val selfValue =
//        if (opened.contains(currentValue)) 0 else

      expand(
        graph,
        opened,
        maxTime,
        elapsed,
        explorationQueue.drop(1) ++ siblings.toVector,
        newAccumulator,
        maxDepth - 1,
        visited + currentValue
      )
    }

  case class AccValue(
      reward: Int,
      increment: Int,
      intermediateElephant: Set[String],
      intermediateHuman: Set[String],
      chainElephant: Vector[String],
      chainHuman: Vector[String]
  ) {
    def chainedWith(other: AccValue): AccValue =
      AccValue(
        reward + other.reward,
        increment + other.increment,
        intermediateElephant ++ other.intermediateElephant,
        intermediateHuman ++ other.intermediateHuman,
        chainElephant ++ other.chainElephant,
        chainHuman ++ other.chainHuman
      )
  }

  case class AccKey(
      startElephant: String,
      endElephant: String,
      startHuman: String,
      endHuman: String,
      time: Int
  )

  val enablePrint: Boolean = false
  def print(timeIndex: Int, graph: Graph, acc: Map[AccKey, AccValue]): Unit = {
    val maxIncrement = {
      for {
        iE    <- graph.producingValves
        jE    <- graph.producingValves
        iH    <- graph.producingValves
        jH    <- graph.producingValves
        value <- acc.get(AccKey(iE, jE, iH, jH, timeIndex))
      } yield value.increment
    }.maxOption
    println(s"Time budget: ${timeIndex} maxIncrement: ${maxIncrement}")

    if(enablePrint) {
      for {
        iE    <- graph.producingValves
        jE    <- graph.producingValves
        iH    <- graph.producingValves
        jH    <- graph.producingValves
        value <- acc.get(AccKey(iE, jE, iH, jH, timeIndex))
        if iE == "AA" && iH == "AA"
      } println(s"$timeIndex:$iE,$jE,$iH,$jH -> ${value.reward} +${value.increment} [${value.chainElephant.mkString(",")}] [${value.chainHuman.mkString(",")}]")
    }

    println("")
  }

  def maxReward3(graph: Graph, maxTime: Int, acc: Map[AccKey, AccValue], timeBudget: Int): Map[AccKey, AccValue] =
    if (timeBudget == maxTime + 1) {
      acc
    } else {
      val newAcc = {
        val nodes = for {
          iE <- graph.producingValves
          jE <- graph.producingValves
          iH <- graph.producingValves
          jH <- graph.producingValves
        } yield (iE, jE, iH, jH)

        val debugTime   = 8
        val debugIE = "AA"
        val debugJE = "DD"
        val debugIH = "AA "
        val debugJH = "CC"

        nodes.map { case (iE, jE, iH, jH) =>
          val costsE = graph.transportCosts(iE)
          val costsH = graph.transportCosts(iH)

          val intermediatePath =
            for {
              kE <- graph.producingValves
              if kE != jE
              kH <- graph.producingValves
              if kH != jH
              if kE != kH
              iToKE <- costsE.get(kE)
              if iToKE < timeBudget
              iToKH <- costsH.get(kH)
              if iToKH < timeBudget
              kToJ <- acc.get(AccKey(kE, jE, kH, jH, timeBudget - math.max(iToKE, iToKH)))
              if !kToJ.intermediateElephant.contains(kE) && !kToJ.intermediateHuman.contains(kE)
              if !kToJ.intermediateElephant.contains(kH) && !kToJ.intermediateHuman.contains(kH)
            } yield {
              val newReward =
                graph.valves(kE) * (timeBudget - iToKE) + graph.valves(kH) * (timeBudget - iToKH) + kToJ.reward
              val newIncrement = kToJ.increment + graph.valves(kE) + graph.valves(kH)
              val r = AccValue(
                newReward,
                newIncrement,
                kToJ.intermediateElephant + kE,
                kToJ.intermediateHuman + kH,
                kToJ.chainElephant.prepended(kE),
                kToJ.chainHuman.prepended(kH)
              )
//              if (timeBudget == debugTime && iValve == debugiValve && jValve == debugjValve) {
//                //                if(kValve=="BB" && iToK._1.time == 5)
//                println(s"mihai-direct $timeBudget,$iValve,$jValve,$kValve[${iToK}] -> (${newReward}, ${newIncrement})")
//              }
              r
            }

          val timebackIntermediatePaths = {
            val previousPaths =
              for {
                kE <- graph.producingValves.par
                if kE != jE
                kH <- graph.producingValves
                if kH != jH
                backInTime <- math.max(0, timeBudget-15) until timeBudget
                accKey = AccKey(iE, kE, iH, kH, backInTime)
                if acc.contains(accKey)
              } yield accKey -> acc(accKey)

            val combined = for {
              iToK <- previousPaths
              kE = iToK._1.endElephant
              kH = iToK._1.endHuman
              kToJ <- acc.get(AccKey(kE, jE, kH, jH, timeBudget - iToK._1.time))
              if iToK._2.intermediateElephant
                .union(iToK._2.intermediateHuman)
                .intersect(
                  kToJ.intermediateElephant.union(kToJ.intermediateHuman)
                )
                .isEmpty
            } yield {
              val newReward    = iToK._2.reward + iToK._2.increment * (timeBudget - iToK._1.time) + kToJ.reward
              val newIncrement = iToK._2.increment + kToJ.increment
              val r = AccValue(
                newReward,
                newIncrement,
                iToK._2.intermediateElephant ++ kToJ.intermediateElephant,
                iToK._2.intermediateHuman ++ kToJ.intermediateHuman,
                iToK._2.chainElephant ++ kToJ.chainElephant,
                iToK._2.chainHuman ++ kToJ.chainHuman
              )
//              if (timeBudget == debugTime && iValve == debugiValve && jValve == debugjValve) {
////                if(kValve=="BB" && iToK._1.time == 5)
//                println(
//                  s"mihai $timeBudget,$iValve,$jValve,$kValve[${iToK._1.time}] -> (${newReward}, ${newIncrement})"
//                )
//              }
              r
            }
            combined
          }

          val directPath = {
            for {
              oldPath <- acc.get(AccKey(iE, jE, iH, jH, timeBudget - 1))
            } yield {
              val newPath = oldPath.copy(reward = oldPath.reward + oldPath.increment)
//              if (timeBudget == debugTime && iValve == debugiValve && jValve == debugjValve) {
//                //                if(kValve=="BB" && iToK._1.time == 5)
//                println(
//                  s"mihai-self $timeBudget,$iValve,$jValve[${oldPath.reward}] -> (${oldPath.reward + oldPath.increment}, ${oldPath.increment})"
//                )
//              }
              newPath
            }
          }.orElse(
            // we spawn a new path if the budget allows
            for {
              costE <- costsE.get(jE)
              costH <- costsH.get(jH)
              if jE != jH
              if math.max(costE, costH) <= timeBudget
            } yield {
              val rewardE = graph.valves(jE) * (timeBudget - costE)
              val rewardH = graph.valves(jH) * (timeBudget - costH)
              val newIncrement = graph.valves(jE) + graph.valves(jH)
              AccValue(
                rewardE + rewardH,
                newIncrement,
                Set(jE),
                Set(jH),
                Vector(jE),
                Vector(jH)
              )
            }
          )

          val bestPath =
            Array(intermediatePath, ParArray(directPath).flatten, timebackIntermediatePaths).flatten.maxByOption(
              accValue => accValue.reward * accValue.increment
            )
          val r = bestPath.map(v => AccKey(iE, jE, iH, jH, timeBudget) -> v)
          r
        }
      }.flatten.toMap

      val resultAcc = acc ++ newAcc
      print(timeBudget, graph, resultAcc)

      maxReward3(graph, maxTime, acc ++ newAcc, timeBudget + 1)
    }
}

object Day16_Problem2_DP extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val graph = {
      val graph = parse(inputFile)

      val transportCosts = for {
        i <- graph.valves.keys
      } yield i -> expand(graph, Set.empty, 30, 0, Vector((i, 0)), Map.empty, 30, Set.empty).map(v =>
        (v._1, v._2.travelCost)
      )

      val newGraph = graph.copy(
        transportCosts = transportCosts.toMap
      )
      newGraph
    }

    val initialAcc: Map[AccKey, AccValue] = {
      for {
        e <- graph.producingValves
        h <- graph.producingValves
        if e != h
        costE = 1
        costH = 1
        if costE > 0 || costH > 0
      } yield {
        val increment = graph.valves(e) * costE + graph.valves(h) * costH
        AccKey(e, e, h, h, 1) -> AccValue(
          0,
          increment,
          if (costE > 0) Set(e) else Set.empty,
          if (costH > 0) Set(h) else Set.empty,
          if (costE > 0) Vector(e) else Vector.empty,
          if (costH > 0) Vector(e) else Vector.empty
        )
      }
    }.toMap

    val maxTime = 26
    print(1, graph, initialAcc)
    val allRewards = maxReward3(graph, maxTime, initialAcc, 2)

    val lastTimes = {
      for {
        s <- Array("AA")
        e <- graph.producingValves
        h <- graph.producingValves
        if e != h
        reward <- allRewards.get(AccKey(s, e, s, h, maxTime))
      } yield reward
    }.maxBy(v => v.reward)

    lastTimes.reward.toString
  }
}
