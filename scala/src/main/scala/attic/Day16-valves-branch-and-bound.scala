package attic
import common._

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.util.Random

object Day16_Problem1_Branch_Bound extends MainBaseBig(16) {

  val maxTime = 30

  case class Graph(
      arches: Map[String, Array[String]],
      valves: Map[String, Int]
  ) {
    val transportCosts: Map[String, Map[String, Int]] = {
      for {
        i <- valves.keys
        if valves(i) > 0 || i == "AA"
      } yield i -> expand2(this, i)
    }.toMap

    private def expand2(graph: Graph, startNode: String) = {
      @tailrec
      def impl(
          explorationQueue: Vector[(String, Int)],
          accumulator: Map[String, Int],
          visited: Set[String]
      ): Map[String, Int] =
        if (explorationQueue.isEmpty)
          accumulator
        else {
          val (currentValue, cost) = explorationQueue.head

          val siblings = graph
            .arches(currentValue)
            .filter(v => !visited.contains(v))
//            .filter(v => !explorationQueue.contains(v))
            .map(v => (v, cost + 1))

          val newAccumulator = accumulator.updated(currentValue, cost + 1)

          impl(
            explorationQueue.drop(1) ++ siblings.toVector,
            newAccumulator,
            visited + currentValue
          )
        }

      val initialQueue = Vector((startNode, 0))

      impl(initialQueue, Map.empty, Set.empty)
    }
  }

  case class Branch(
      chain: Vector[String],
      elapsedTime: Int,
      rewardToTime: Int,
      increment: Int,
      heuristic: BigInt,
      maxPossibleReward: Int
  ) {
    override def toString: String =
      s"${chain}: rwrd: ${rewardToTime} incr: ${increment} maxReward: ${maxPossibleReward} elapsed: ${elapsedTime}"
  }

  override def run(inputFile: List[String]): String = {
    val graph = parse(inputFile)

    val solution = branchAndBound(graph)

    println(solution)
    solution.rewardToTime.toString
  }

  private def branchAndBound(graph: Graph): Branch = {

    val producingValves: TreeSet[String] = {
      implicit val o: Ordering[String] = (a: String, b: String) => graph.valves(b) - graph.valves(a)

      val values = graph.valves.keys.filter(v => graph.valves(v) > 0).toList
      TreeSet[String](values: _*)
    }

    def computeMaximumReward(nodes: TreeSet[String], timeSpan: Int): Int =
      nodes.take(timeSpan - 1).zipWithIndex.foldLeft(0) { case (acc, (v, index)) =>
        acc + (timeSpan - index - 1) * graph.valves(v)
      }

    implicit val ordering: Ordering[Branch] = (a: Branch, b: Branch) =>
      if (a.increment < b.increment) -1 else if (a.increment == b.increment) 0 else 1

    val explorationQueue = mutable.PriorityQueue[Branch]()

    @tailrec
    def impl(explorationQueue: mutable.PriorityQueue[Branch], currentMax: Option[Branch]): Option[Branch] =
      if (explorationQueue.isEmpty) {
        currentMax
      } else {
        val path = explorationQueue.head

        if (path.maxPossibleReward < currentMax.map(_.rewardToTime).getOrElse(0)) {
//          println(s"discarding ${path} vs $currentMax")
          impl(explorationQueue.drop(1), currentMax)
        } else {
          // extend path and reinject
          val freeNodes = producingValves
            .diff(path.chain.toSet)
            .filter(n => graph.transportCosts(path.chain.last).contains(n))

          val newExploration = freeNodes.toList.map { nextNode =>
            if (path.elapsedTime + graph.transportCosts(path.chain.last)(nextNode) > maxTime) {
              None
            } else {
              val rewardToTime    = path.rewardToTime + graph.transportCosts(path.chain.last)(nextNode) * path.increment
              val elapsedTime     = path.elapsedTime + graph.transportCosts(path.chain.last)(nextNode)
              val remainingReward = computeMaximumReward(freeNodes - nextNode, maxTime - elapsedTime)
              val increment       = path.increment + graph.valves(nextNode)
              val branch = Branch(
                path.chain.appended(nextNode),
                elapsedTime = elapsedTime,
                rewardToTime = rewardToTime,
                increment = increment,
                heuristic = (if (rewardToTime == 0) BigInt(1) else BigInt(rewardToTime)) * remainingReward,
                maxPossibleReward = rewardToTime + (maxTime - elapsedTime) * increment + remainingReward
              )
              Some(branch)
            }
          }.flatten

          val newMax = if (newExploration.isEmpty) {
            // we have a solution so expand the reward to max time
            val reward = path.rewardToTime + (maxTime - path.elapsedTime) * path.increment
            val finalizedPath = path.copy(
              rewardToTime = path.rewardToTime + (maxTime - path.elapsedTime) * path.increment,
              increment = path.increment,
              elapsedTime = maxTime,
              maxPossibleReward = reward
            )

            currentMax.filter(max => max.rewardToTime > finalizedPath.rewardToTime).orElse(Some(finalizedPath))
          } else {
            currentMax
          }

          impl(explorationQueue.drop(1) ++ newExploration, newMax)
        }
      }

    val firstBranch = Branch(
      chain = Vector("AA"),
      elapsedTime = 0,
      rewardToTime = 0,
      increment = 0,
      heuristic = 0,
      maxPossibleReward = 0
    )

    val solution = impl(explorationQueue ++ List(firstBranch), None)
    solution.get
  }

  def parse(input: List[String]): Graph = {
    val (siblings, values) = input.map { l =>
      val arr    = l.split(";")
      val selfRe = "Valve (\\w+) has flow rate=(\\d+)".r
      val thisNode = arr(0) match {
        case selfRe(thisValve, thisValue) => (thisValve, thisValue.toInt)
      }
      val siblings =
        arr(1).replace(" tunnels lead to valves ", "").replace(" tunnel leads to valve ", "").split(",").map(_.trim)

      (thisNode._1 -> thisNode._2, thisNode._1 -> siblings)
    }
      .foldLeft((Map.empty[String, Array[String]], Map.empty[String, Int])) { case (acc, (valveSpec, childrenSpec)) =>
        (acc._1 + childrenSpec, acc._2 + valveSpec)
      }

    Graph(siblings, values)
  }
}
