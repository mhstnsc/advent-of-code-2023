package attic
import Day16_Problem1_Branch_Bound._
import common._

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.util.Random

// 2400 is the right answer ??? really?
// the algorithm outputs 2399 but the correct answer is 2400 ....no way
object Day16_Problem2_Branch_Bound extends MainBaseBig(16) {

  val maxTime = 26

  case class Branch(
      chain: Vector[String],
      chainTimes: Vector[Int],
      elapsedTime: Int,
      rewardToTime: Int,
      increment: Int
  ) {
    override def toString: String =
      s"${chain.zip(chainTimes).map(s => s"${s._1}-${s._2}").mkString(" ")}: rwrd: ${rewardToTime} incr: ${increment} elapsed: ${elapsedTime}"

    def extrapolated(time: Int): Branch = {
      require(time >= elapsedTime)
      this.copy(
        elapsedTime = time,
        rewardToTime = rewardToTime + (time - elapsedTime) * increment
      )
    }

    def appended(node: String, cost: Int, newIncrement: Int): Branch =
      Branch(
        chain = chain.appended(node),
        chainTimes = chainTimes.appended(elapsedTime + cost),
        elapsedTime = elapsedTime + cost,
        rewardToTime = rewardToTime + increment * cost,
        increment = increment + newIncrement
      )
  }

  case class DoubleBranch(
      elephant: Branch,
      human: Branch,
      maxPossibleReward: Int
  ) {
    def rewardToTime: Int = {
      val latestTime = math.max(elephant.elapsedTime, human.elapsedTime)
      elephant.rewardToTime + human.rewardToTime +
        elephant.increment * (latestTime - elephant.elapsedTime) +
        human.increment * (latestTime - human.elapsedTime)
    }

    override def toString: String =
      s"rwrd: $rewardToTime ; [$elephant] ; [$human]; "
  }

  override def run(inputFile: List[String]): String = {
    val graph = parse(inputFile)

    println(graph.valves.values.sum)

    val solution = branchAndBound(graph)

    println(solution)
    solution.rewardToTime.toString
  }

  private def branchAndBound(graph: Graph): DoubleBranch = {

    // ordered descending by increment
    val producingValves: TreeSet[String] = {
      implicit val o: Ordering[String] = (a: String, b: String) =>
        graph.valves.getOrElse(b, 0) - graph.valves.getOrElse(a, 0)

      val values = graph.valves.keys.filter(v => graph.valves(v) > 0).toList
      TreeSet[String](values: _*)
    }

    val medianTravelTime: Map[String, Int] =
      producingValves.toList.map { s =>
        graph
          .transportCosts(s)
          .map { case (d, cost) =>
            d -> cost
          }
          .filter(_._2 > 0)
      }.flatten.groupBy(_._1).map { l =>
        val x = l._2.map(_._2).sorted
        l._1 -> x.min
      }

    def estimateRemainingReward(nodes: TreeSet[String], timeSpan: Int): Int = {
      val newNodes = if (nodes.size % 2 == 1) {
        nodes + "ZZZZZZ"
      } else nodes

//      val specialSorting =
//        newNodes.toVector
//          .sortBy(l => graph.valves.getOrElse(l, 0).toDouble / medianTravelTime.getOrElse(l, 1))(
//            Ordering[Double].reverse
//          )

      newNodes.sliding(2, 2).take(timeSpan - 1).zipWithIndex.foldLeft(0) { case (acc, (t, index)) =>
        acc +
          (timeSpan - index - 1) * graph.valves(t.toIndexedSeq(0)) +
          (timeSpan - index - 1) * graph.valves.getOrElse(t.toIndexedSeq(1), graph.valves(t.toIndexedSeq(0)))
      }
    }

    implicit val ordering: Ordering[DoubleBranch] = (a: DoubleBranch, b: DoubleBranch) => {
      val maxTime = Array(a.elephant.elapsedTime, a.human.elapsedTime, b.elephant.elapsedTime, b.human.elapsedTime).max
      val aValue  = a.elephant.extrapolated(maxTime).rewardToTime + a.human.extrapolated(maxTime).rewardToTime
      val bValue  = b.elephant.extrapolated(maxTime).rewardToTime + b.human.extrapolated(maxTime).rewardToTime
      if (aValue < bValue) -1 else if (aValue == bValue) 0 else 1
    }

    val explorationQueue = mutable.PriorityQueue[DoubleBranch]()

    var discarded = BigInt(0)

    def spawn(path: DoubleBranch): Seq[DoubleBranch] = {
      // extend path and reinject
      val freeNodes = producingValves
        .diff(path.elephant.chain.toSet ++ path.human.chain.toSet)

      freeNodes.toList.flatMap { nextNode =>
        if (
          path.elephant.elapsedTime + graph.transportCosts(path.elephant.chain.last)(nextNode) > maxTime &&
          path.human.elapsedTime + graph.transportCosts(path.human.chain.last)(nextNode) > maxTime
        ) {
          Nil
        } else {
          val updateElephant =
            if (path.elephant.elapsedTime + graph.transportCosts(path.elephant.chain.last)(nextNode) > maxTime)
              None
            else
              Some(
                path.elephant.appended(
                  nextNode,
                  graph.transportCosts(path.elephant.chain.last)(nextNode),
                  graph.valves(nextNode)
                )
              )

          val updateHuman =
            if (path.human.elapsedTime + graph.transportCosts(path.human.chain.last)(nextNode) > maxTime)
              None
            else
              Some(
                path.human.appended(
                  nextNode,
                  graph.transportCosts(path.human.chain.last)(nextNode),
                  graph.valves(nextNode)
                )
              )

          val candidates = if (path.elephant.elapsedTime < path.human.elapsedTime) {
            List(updateElephant.map(v => (v, path.human)))
          } else {
            List(updateHuman.map(v => (path.elephant, v)))
          }

//          val candidates = List(
//            updateElephant.map(v => (v, path.human)),
//            updateHuman.map(v => (path.elephant, v))
//          )

          candidates.flatten.map { case (e, h) =>
            val remainingReward = estimateRemainingReward(
              freeNodes - nextNode,
              maxTime - math.min(e.elapsedTime, h.elapsedTime)
            )
            val branch = DoubleBranch(
              e,
              h,
              maxPossibleReward = e.extrapolated(maxTime).rewardToTime +
                h.extrapolated(maxTime).rewardToTime +
                remainingReward
            )
            branch
          }
        }
      }
    }

    val globalMax = new AtomicReference[Option[DoubleBranch]]()
    globalMax.set(None)

    def parImpl(
        path: DoubleBranch
    ): Option[DoubleBranch] = {
      val r = spawn(path).par.map { v =>
        impl(
          new mutable.PriorityQueue[DoubleBranch]() ++ List(v),
          None,
          Random.nextInt(500000)
        )
      }.flatten

      if (r.isEmpty) {
        None
      } else {
        Some(r.maxBy(v => v.maxPossibleReward))
      }
    }

    @tailrec
    def impl(
        explorationQueue: mutable.PriorityQueue[DoubleBranch],
        currentMax: Option[DoubleBranch],
        explored: Int
    ): Option[DoubleBranch] = {

      val newCurrentMax = if (explored % 500000 == 0) {
        globalMax.updateAndGet(v =>
          v.filter(p => p.maxPossibleReward > currentMax.map(_.maxPossibleReward).getOrElse(0)).orElse(currentMax)
        )
      } else {
        currentMax
      }

      if (explored % 500000 == 0) {
        println(
          s"${explored} discarded=$discarded perc = ${discarded * 100 / explored} queue=${explorationQueue.size} currentMax: ${currentMax}"
        )
      }

      if (explorationQueue.isEmpty) {
        newCurrentMax
      } else {
        val path = explorationQueue.dequeue()

        if (path.maxPossibleReward < newCurrentMax.map(_.maxPossibleReward).getOrElse(0)) {
//          println(s"discarding ${path} vs $currentMax")
          discarded = discarded + 1
          impl(explorationQueue, currentMax, explored + 1)
        } else {
          // extend path and reinject
          val freeNodes = producingValves
            .diff(path.elephant.chain.toSet ++ path.human.chain.toSet)

          val newExploration = freeNodes.toList.flatMap { nextNode =>
            if (
              path.elephant.elapsedTime + graph.transportCosts(path.elephant.chain.last)(nextNode) > maxTime &&
              path.human.elapsedTime + graph.transportCosts(path.human.chain.last)(nextNode) > maxTime
            ) {
              Nil
            } else {
              val updateElephant =
                if (path.elephant.elapsedTime + graph.transportCosts(path.elephant.chain.last)(nextNode) > maxTime)
                  None
                else
                  Some(
                    path.elephant.appended(
                      nextNode,
                      graph.transportCosts(path.elephant.chain.last)(nextNode),
                      graph.valves(nextNode)
                    )
                  )

              val updateHuman =
                if (path.human.elapsedTime + graph.transportCosts(path.human.chain.last)(nextNode) > maxTime)
                  None
                else
                  Some(
                    path.human.appended(
                      nextNode,
                      graph.transportCosts(path.human.chain.last)(nextNode),
                      graph.valves(nextNode)
                    )
                  )

              val candidates = if (path.elephant.elapsedTime < path.human.elapsedTime) {
                List(updateElephant.map(v => (v, path.human)))
              } else if (path.elephant.elapsedTime > path.human.elapsedTime) {
                List(updateHuman.map(v => (path.elephant, v)))
              } else {
                List(
                  updateElephant.map(v => (v, path.human)),
                  updateHuman.map(v => (path.elephant, v))
                )
              }

              //          val candidates = List(
              //            updateElephant.map(v => (v, path.human)),
              //            updateHuman.map(v => (path.elephant, v))
              //          )

              candidates.flatten.map { case (e, h) =>
                val remainingReward = estimateRemainingReward(
                  freeNodes - nextNode,
                  maxTime - math.min(e.elapsedTime, h.elapsedTime)
                )
                val branch = DoubleBranch(
                  e,
                  h,
                  maxPossibleReward = e.extrapolated(maxTime).rewardToTime +
                    h.extrapolated(maxTime).rewardToTime +
                    remainingReward
                )
                branch
              }
            }
          }

          val newMax = if (newExploration.isEmpty) {
            // we have a solution so expand the reward to max time

            val updatedElephant = path.elephant.extrapolated(maxTime)
            val updatedHuman    = path.human.extrapolated(maxTime)

            val finalizedPath = DoubleBranch(
              updatedElephant,
              updatedHuman,
              maxPossibleReward = updatedElephant.rewardToTime + updatedHuman.rewardToTime
            )

            require(
              (finalizedPath.human.chain zip finalizedPath.human.chainTimes).foldLeft(0) { case (acc, (v, time)) =>
                acc + graph.valves(v) * (maxTime - time)
              } + (finalizedPath.elephant.chain zip finalizedPath.elephant.chainTimes).foldLeft(0) {
                case (acc, (v, time)) =>
                  acc + graph.valves(v) * (maxTime - time)
              } == finalizedPath.maxPossibleReward
            )

            require(
              finalizedPath.human.elapsedTime == maxTime &&
                finalizedPath.elephant.elapsedTime == maxTime
            )

            require(
              finalizedPath.maxPossibleReward == finalizedPath.rewardToTime
            )

//            println(finalizedPath.maxPossibleReward + " " + finalizedPath.rewardToTime)

            newCurrentMax
              .filter(max => max.maxPossibleReward > finalizedPath.maxPossibleReward)
              .orElse(Some(finalizedPath))
          } else {
            newCurrentMax
          }

          impl(explorationQueue ++ newExploration, newMax, explored + 1)
        }
      }
    }

    val firstBranch = Branch(
      chain = Vector("AA"),
      chainTimes = Vector(0),
      elapsedTime = 0,
      rewardToTime = 0,
      increment = 0
    )
    val firstDoubleBrach = DoubleBranch(
      firstBranch,
      firstBranch,
      0
    )

    val solution = parImpl(firstDoubleBrach)
//    val solution = impl(explorationQueue ++ List(firstDoubleBrach), None)
    solution.get
  }
}
