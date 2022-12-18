package attic

import attic.Day16._
import common._

import scala.annotation.tailrec

object Day16 {
  val day = 16

  case class Graph(
      arches: Map[String, Array[String]],
      valves: Map[String, Int],
      transportCosts: Map[String, Map[String, Int]] // for each node we have a map to a destination node with a cost
  )

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

  case class Reward(accumulatedReward: Int, previousValve: String, cost: Int) {
//    def efficiency: Int = if (cost == 0) 0 else accumulatedReward / cost
    def dijkstra: Int = if (accumulatedReward == 0) Int.MaxValue else -accumulatedReward
//    def dijkstra2: Int  = if (cost == 0) 0 else -accumulatedReward / cost
  }

  def accumulateOpened(currentValve: String, accumulatedResults: Map[String, Reward]): Set[String] =
    if (currentValve == accumulatedResults(currentValve).previousValve) {
      Set(currentValve)
    } else {
      Set(currentValve) ++ accumulateOpened(accumulatedResults(currentValve).previousValve, accumulatedResults)
    }

  def maxReward2(
      graph: Graph,
      maxTime: Int,
      accumulatedRewards: Map[String, Reward],
      evaluationSet: Set[String]
  ): Map[String, Reward] =
    if (evaluationSet.isEmpty) {
      accumulatedRewards
    } else {
      // pick the max rewarding from the evaluation set
      val currentValve = evaluationSet.minBy(v => accumulatedRewards(v).dijkstra)
      val currentCost  = accumulatedRewards(currentValve).cost
      val opened       = accumulateOpened(currentValve, accumulatedRewards)

      val expanded = expand(
        graph,
        opened,
        maxTime,
        currentCost,
        Vector((currentValve, 0)),
        Map.empty,
        maxTime - currentCost - 1,
        Set.empty
      )

      // update accumulated rewards with the expansion
      val newAccumulatedRewards =
        if (expanded.isEmpty) accumulatedRewards
        else
          accumulatedRewards.map { case (nextValve, reward) =>
            (
              nextValve,
              if (expanded.contains(nextValve)) {
                val intermediateValve = accumulatedRewards(currentValve)
                val expansionData     = expanded(nextValve)

                val newPathReward = intermediateValve.accumulatedReward + expansionData.accumulatedReward
//                  math.max(0, maxTime - expansionData.travelCost - intermediateValve.cost) * graph.valves(nextValve)
                val newPathCost = intermediateValve.cost + expansionData.travelCost

                val newReward = Reward(newPathReward, currentValve, newPathCost)

                if (newReward.dijkstra < reward.dijkstra) {
                  Reward(newPathReward, currentValve, newPathCost)
                } else if (reward.dijkstra == newReward.dijkstra && reward.cost > newReward.cost) {
                  newReward
                } else {
                  reward
                }

//                if (reward.efficiency < newPathReward / newPathCost) {
//                  Reward(newPathReward, currentValve, newPathCost)
//                } else if (reward.efficiency == newPathReward / newPathCost && reward.cost < newPathCost) {
//                  Reward(newPathReward, currentValve, newPathCost)
//                } else {
//                  reward
//                }
              } else {
                reward
              }
            )
          }

      maxReward2(
        graph,
        maxTime,
        newAccumulatedRewards,
        evaluationSet - currentValve
      )
    }

  case class AccValue(
      reward: Int,
      increment: Int,
      intermediateValves: Set[String],
      chainOfValves: Vector[String]
  ) {
    def chainedWith(other: AccValue): AccValue =
      AccValue(
        reward + other.reward,
        increment + other.increment,
        intermediateValves ++ other.intermediateValves,
        chainOfValves ++ other.chainOfValves
      )
  }

  case class AccKey(
      startValve: String,
      endValve: String,
      time: Int
  )

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
          .map(v => (v.reward, v.increment, v.chainOfValves.mkString(",")).toString())
          .getOrElse("X")
      )
    }
    println(printMatrix.mkString(" "))
    println("")
  }

  def maxReward3(graph: Graph, maxTime: Int, acc: Map[AccKey, AccValue], timeBudget: Int): Map[AccKey, AccValue] =
    if (timeBudget == maxTime + 1) {
      acc
    } else {
      val newAcc = {
        val nodes = for {
          i <- graph.valves.keys.toList.sorted
          j <- graph.valves.keys.toList.sorted
        } yield (i, j)

        val debugTime = 8
        val debugiValve = "AA"
        val debugjValve = "JJ"

        nodes.map { case (iValve, jValve) =>
          val costs = graph.transportCosts(iValve)

          val intermediatePath =
            for {
              kValve <- graph.valves.keys.toList.sorted
              if kValve != jValve
              iToK <- costs.get(kValve)
              if iToK < timeBudget
              kToJ <- acc.get(AccKey(kValve, jValve, timeBudget - iToK))
              if !kToJ.intermediateValves.contains(kValve)
            } yield {
              val newReward = graph.valves(kValve) * (timeBudget - iToK) + kToJ.reward
              val newIncrement =  kToJ.increment + graph.valves(kValve)
              val r = AccValue(
                newReward,
                newIncrement,
                kToJ.intermediateValves + kValve,
                kToJ.chainOfValves.prepended(kValve)
              )
              if(timeBudget == debugTime && iValve==debugiValve && jValve==debugjValve) {
                //                if(kValve=="BB" && iToK._1.time == 5)
                println(s"mihai-direct $timeBudget,$iValve,$jValve,$kValve[${iToK}] -> (${newReward}, ${newIncrement})")
              }
              r
            }

          val timebackIntermediatePaths = {
            val previousPaths =
              for {
                kValve <- graph.valves.keys.toList.sorted
                if kValve != jValve
                backInTime <- 1 until timeBudget
                accKey = AccKey(iValve, kValve, backInTime)
                if acc.contains(accKey)
              } yield accKey -> acc(accKey)

            val combined = for {
              iToK <- previousPaths
              kValve = iToK._1.endValve
              kToJ <- acc.get(AccKey(kValve, jValve, timeBudget - iToK._1.time))
              if iToK._2.intermediateValves.intersect(kToJ.intermediateValves).isEmpty
            } yield {
              val newReward = iToK._2.reward + iToK._2.increment * (timeBudget - iToK._1.time) + kToJ.reward
              val newIncrement = iToK._2.increment + kToJ.increment
              val r = AccValue(
                newReward,
                newIncrement,
                iToK._2.intermediateValves ++ kToJ.intermediateValves,
                iToK._2.chainOfValves ++ kToJ.chainOfValves
              )
              if(timeBudget == debugTime && iValve==debugiValve && jValve==debugjValve) {
//                if(kValve=="BB" && iToK._1.time == 5)
                println(s"mihai $timeBudget,$iValve,$jValve,$kValve[${iToK._1.time}] -> (${newReward}, ${newIncrement})")
              }
              r
            }
            combined
          }

          val directPath = {
            {
              for {
                oldPath <- acc.get(AccKey(iValve, jValve, timeBudget - 1))
              } yield {
                val newPath = oldPath.copy(reward = oldPath.reward + oldPath.increment)
                if(timeBudget == debugTime && iValve==debugiValve && jValve==debugjValve) {
                  //                if(kValve=="BB" && iToK._1.time == 5)
                  println(s"mihai-self $timeBudget,$iValve,$jValve[${oldPath.reward}] -> (${oldPath.reward + oldPath.increment}, ${oldPath.increment})")
                }
                newPath
              }
            }.orElse(
                // we spawn a new path if the budget allows
                costs
                  .get(jValve)
                  .filter(v => v <= timeBudget)
                  .map(_ => AccValue(0, graph.valves(jValve), Set(jValve), Vector(jValve)))
              )
          }


          val bestPath = Array(intermediatePath, directPath, timebackIntermediatePaths).flatten.maxByOption(accValue =>
            accValue.reward * accValue.increment
          )
          val r = bestPath.map(v => AccKey(iValve, jValve, timeBudget) -> v)
          r
        }
      }.flatten.toMap

      val resultAcc = acc ++ newAcc
//      print(timeBudget, graph, resultAcc)

      maxReward3(graph, maxTime, acc ++ newAcc, timeBudget + 1)
    }

  def maxReward4(graph: Graph, maxTime: Int, acc: Map[AccKey, AccValue], timeBudget: Int): Map[AccKey, AccValue] =
    if (timeBudget == maxTime + 1) {
      acc
    } else {
      val newAcc = {
        val nodes = for {
          i <- graph.valves.keys.toList.sorted
          j <- graph.valves.keys.toList.sorted
        } yield (i, j)

        val debugTime = 8
        val debugiValve = "AA"
        val debugjValve = "JJ"

        nodes.map { case (iValve, jValve) =>
          val costs = graph.transportCosts(iValve)

          val intermediatePath =
            for {
              kValve <- graph.valves.keys.toList.sorted
              if kValve != jValve
              iToK <- costs.get(kValve)
              if iToK < timeBudget
              kToJ <- acc.get(AccKey(kValve, jValve, timeBudget - iToK))
              if !kToJ.intermediateValves.contains(kValve)
            } yield {
              val newReward = graph.valves(kValve) * (timeBudget - iToK) + kToJ.reward
              val newIncrement =  kToJ.increment + graph.valves(kValve)
              val r = AccValue(
                newReward,
                newIncrement,
                kToJ.intermediateValves + kValve,
                kToJ.chainOfValves.prepended(kValve)
              )
              if(timeBudget == debugTime && iValve==debugiValve && jValve==debugjValve) {
                //                if(kValve=="BB" && iToK._1.time == 5)
                println(s"mihai-direct $timeBudget,$iValve,$jValve,$kValve[${iToK}] -> (${newReward}, ${newIncrement})")
              }
              r
            }

          val timebackIntermediatePaths = {
            val previousPaths =
              for {
                kValve <- graph.valves.keys.toList.sorted
                if kValve != jValve
                backInTime <- 1 until timeBudget
                accKey = AccKey(iValve, kValve, backInTime)
                if acc.contains(accKey)
              } yield accKey -> acc(accKey)

            val combined = for {
              iToK <- previousPaths
              kValve = iToK._1.endValve
              kToJ <- acc.get(AccKey(kValve, jValve, timeBudget - iToK._1.time))
              if iToK._2.intermediateValves.intersect(kToJ.intermediateValves).isEmpty
            } yield {
              val newReward = iToK._2.reward + iToK._2.increment * (timeBudget - iToK._1.time) + kToJ.reward
              val newIncrement = iToK._2.increment + kToJ.increment
              val r = AccValue(
                newReward,
                newIncrement,
                iToK._2.intermediateValves ++ kToJ.intermediateValves,
                iToK._2.chainOfValves ++ kToJ.chainOfValves
              )
              if(timeBudget == debugTime && iValve==debugiValve && jValve==debugjValve) {
                //                if(kValve=="BB" && iToK._1.time == 5)
                println(s"mihai $timeBudget,$iValve,$jValve,$kValve[${iToK._1.time}] -> (${newReward}, ${newIncrement})")
              }
              r
            }
            combined
          }

          val directPath = {
            {
              for {
                oldPath <- acc.get(AccKey(iValve, jValve, timeBudget - 1))
              } yield {
                val newPath = oldPath.copy(reward = oldPath.reward + oldPath.increment)
                if(timeBudget == debugTime && iValve==debugiValve && jValve==debugjValve) {
                  //                if(kValve=="BB" && iToK._1.time == 5)
                  println(s"mihai-self $timeBudget,$iValve,$jValve[${oldPath.reward}] -> (${oldPath.reward + oldPath.increment}, ${oldPath.increment})")
                }
                newPath
              }
            }.orElse(
              // we spawn a new path if the budget allows
              costs
                .get(jValve)
                .filter(v => v <= timeBudget)
                .map(_ => AccValue(0, graph.valves(jValve), Set(jValve), Vector(jValve)))
            )
          }


          val bestPath = Array(intermediatePath, directPath, timebackIntermediatePaths).flatten.maxByOption(accValue =>
            accValue.reward + accValue.increment* (maxTime - timeBudget)
          )
          val r = bestPath.map(v => AccKey(iValve, jValve, timeBudget) -> v)
          r
        }
      }.flatten.toMap

      val resultAcc = acc ++ newAcc
      //      print(timeBudget, graph, resultAcc)

      maxReward3(graph, maxTime, acc ++ newAcc, timeBudget + 1)
    }

}

object Day16_Problem1_DP extends MainBaseSmall(day) {
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
        i <- graph.valves.keys
        j <- graph.valves.keys
        if i == j
      } yield AccKey(i, j, 1) -> AccValue(0, graph.valves(j), Set(j), Vector(j))
    }.toMap

    val maxTime = 30
    print(1, graph, initialAcc)
    val allRewards = maxReward3(graph, maxTime, initialAcc, 2)

    val lastTimes = {
      for {
        i <- List("AA")
        j <- graph.valves.keys
      } yield allRewards(AccKey(i, j, maxTime))
    }.maxBy(v => v.reward)

    lastTimes.reward.toString
  }
}

object Day16_Problem2 extends MainBase(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)

    ""
  }
}

object Day16_Problem1_Small extends MainBaseSmall(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)

    ""
  }
}

object Day16_Problem1_Big extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val input = parse(inputFile)
    ""
  }
}

