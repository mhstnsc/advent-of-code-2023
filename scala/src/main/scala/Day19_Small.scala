import common.InputSyntax.InputSyntax
import common._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable

object Day19_Problem1 extends MainBaseSmall(19) {

  type Ore      = Int
  type Clay     = Int
  type Obsidian = Int
  type Geode    = Int

  case class Blueprint(
      id: Int,
      ore_OreCost: Ore,
      clay_OreCost: Ore,
      obs_OreCost: Ore,
      obs_ClayCost: Clay,
      geo_OreCost: Ore,
      geo_ObsCost: Obsidian
  ) {
    override def toString: String =
      s"ore=$ore_OreCost clay=ore:$clay_OreCost obs=ore:${obs_OreCost},clay:${obs_ClayCost} geo=ore:${geo_OreCost},obs:${geo_ObsCost}]]"
  }

  case class Resource(
      deposit: Int,
      robots: Int
  ) {
    override def toString: String =
      s"$deposit,+${robots}"

    require(deposit >= 0)
    def tick(): Resource =
      this.copy(deposit = deposit + robots)

    def decDeposit(cost: Int): Resource = this.copy(deposit = deposit - cost)

    def incRobots(): Resource = this.copy(robots = robots + 1)
  }

  case class Inventory(
      ore: Resource,
      clay: Resource,
      obsidian: Resource,
      geode: Resource,
      blueprint: Blueprint,
      elapsed: Int,
      firstGeoTime: Int
  ) {
    def greedyScore(): Int =
      (Math.min(1, clay.deposit.toDouble / blueprint.obs_ClayCost) * 100 * clay.robots *
        Math.min(1, obsidian.deposit.toDouble / blueprint.geo_ObsCost) * 100).toInt * obsidian.robots

//    def greedyScore(): Int =
//      obsidian.robots * 1000 + clay.robots * 100 + ore.robots

    override def toString: String =
      s"${elapsed}m -> ore[$ore] clay[$clay] obs[$obsidian] geo[$geode]"

    def tick: Inventory = {
      val newInventory = this.copy(
        ore = ore.tick(),
        clay = clay.tick(),
        obsidian = obsidian.tick(),
        geode = geode.tick(),
        elapsed = elapsed + 1
      )
      if (geode.deposit == 0 && newInventory.geode.deposit == 1) {
        newInventory.copy(firstGeoTime = newInventory.elapsed)
      } else {
        newInventory
      }
    }

    def genChoices(): Seq[Inventory] = {
      val maybeOre = Option.when(ore.deposit >= blueprint.ore_OreCost) {
        this.copy(
          ore = ore.tick().decDeposit(blueprint.ore_OreCost).incRobots(),
          clay = clay.tick(),
          obsidian = obsidian.tick(),
          geode = geode.tick(),
          elapsed = elapsed + 1
        )
      }
      val maybeClay = Option.when(ore.deposit >= blueprint.clay_OreCost) {
        this.copy(
          ore = ore.tick().decDeposit(blueprint.clay_OreCost),
          clay = clay.tick().incRobots(),
          obsidian = obsidian.tick(),
          geode = geode.tick(),
          elapsed = elapsed + 1
        )
      }
      // first determine if we can build a geode and what is missing
      val maybeObsidian = Option.when(clay.deposit >= blueprint.obs_ClayCost && ore.deposit >= blueprint.obs_OreCost) {
        this.copy(
          ore = ore.tick().decDeposit(blueprint.obs_OreCost),
          clay = clay.tick().decDeposit(blueprint.obs_ClayCost),
          obsidian = obsidian.tick().incRobots(),
          geode = geode.tick(),
          elapsed = elapsed + 1
        )
      }

      val maybeGeode = Option.when(ore.deposit >= blueprint.geo_OreCost && obsidian.deposit >= blueprint.geo_ObsCost) {
        this.copy(
          ore = ore.tick().decDeposit(blueprint.geo_OreCost),
          clay = clay.tick(),
          obsidian = obsidian.tick().decDeposit(blueprint.geo_ObsCost),
          geode = geode.tick().incRobots(),
          elapsed = elapsed + 1
        )
      }

      List(maybeOre, maybeClay, maybeGeode, maybeObsidian, Some(this.tick)).flatten
    }
  }

  var solutionCount: Int  = 0
  var discardedCount: Int = 0
  import scala.collection.parallel.CollectionConverters._

  override def run(inputFile: List[String]): String = {
    val blueprints = parse(inputFile)

    val maxTime = 24

    val solutions = blueprints.drop(1).map { b =>
      println(s"solving ${b}")
      val initialInventory = Inventory(
        ore = Resource(0, 1),
        clay = Resource(0, 0),
        obsidian = Resource(0, 0),
        geode = Resource(0, 0),
        elapsed = 0,
        blueprint = b,
        firstGeoTime = 0
      )

//      implicit val greed: Ordering[Inventory] = (a: Inventory, b: Inventory) => a.greedyScore() - b.greedyScore()
      implicit val greed: Ordering[Inventory] = (a: Inventory, b: Inventory) => {
        val blueprint = a.blueprint
        if (a.geode.robots != b.geode.robots) a.geode.robots - b.geode.robots
        else if (
          math.min(blueprint.geo_ObsCost, a.obsidian.robots) != math.min(blueprint.geo_ObsCost, b.obsidian.robots)
        ) math.min(blueprint.geo_ObsCost, a.obsidian.robots) - math.min(blueprint.geo_ObsCost, b.obsidian.robots)
        else if (math.min(blueprint.obs_ClayCost, a.clay.robots) != math.min(blueprint.obs_ClayCost, b.clay.robots))
          math.min(blueprint.obs_ClayCost, a.clay.robots) - math.min(blueprint.obs_ClayCost, b.clay.robots)
        else if (a.geode.deposit != b.geode.deposit) a.geode.deposit - a.geode.deposit
        else if (
          math.min(blueprint.geo_ObsCost, a.obsidian.deposit) != math.min(blueprint.geo_ObsCost, b.obsidian.deposit)
        ) math.min(blueprint.geo_ObsCost, a.obsidian.deposit) - math.min(blueprint.geo_ObsCost, b.obsidian.deposit)
        else if (math.min(blueprint.obs_ClayCost, a.clay.deposit) != math.min(blueprint.obs_ClayCost, b.clay.deposit))
          math.min(blueprint.obs_ClayCost, a.clay.deposit) - math.min(blueprint.obs_ClayCost, b.clay.deposit)
        else a.ore.deposit - b.ore.deposit
      }
      val priorityQueue = new mutable.PriorityQueue[Inventory]()
      val initialMax    = initialInventory.copy(elapsed = maxTime)

      val allSolutions = bfsInvoker(priorityQueue, maxTime, 1, List(initialInventory))

      println(s"solving ${b} ... done with ${allSolutions.map(i => i.geode.deposit).max}")
      (b, allSolutions.map(i => i.geode.deposit).max)
    }

    solutions.map { case (b, geodes) => b.id * geodes }.sum.toString
  }

  def bfsInvoker(
      priorityQueue: mutable.PriorityQueue[Inventory],
      maxTime: Int,
      currentMaxGeode: Int,
      solutions: List[Inventory]
  ): List[Inventory] = {
    val (unexplored, finished) = solutions.partition(i => i.elapsed < 24)

    val mostGeodes = unexplored.maxByOption(i => i.geode.deposit).map(_.geode.deposit)
    val leastTime  = unexplored.minByOption(i => i.elapsed).map(_.elapsed)

    val filteredUnexplored = unexplored.filter { i =>
      mostGeodes.contains(i.geode.deposit) || leastTime.contains(i.elapsed)
    }

    require(priorityQueue.isEmpty)
    priorityQueue.enqueue(filteredUnexplored.distinct: _*)

    if (priorityQueue.isEmpty) {
      solutions
    } else {
      val newCandidates = bfs(priorityQueue, maxTime, currentMaxGeode, Nil, 0)

      val minTime               = newCandidates.minBy(_.firstGeoTime).firstGeoTime
      val newCandidatesFilteted = newCandidates.filter(_.firstGeoTime == minTime)

      val allSolutions = (newCandidates ++ finished).distinct

      priorityQueue.clear()
//      require(priorityQueue.isEmpty)
      bfsInvoker(priorityQueue, maxTime, currentMaxGeode + 1, allSolutions)
    }

  }

//  @tailrec
//  def fullBfs(
//      queue: mutable.PriorityQueue[Inventory],
//      maxTime: Int,
//      maxGeode: Int,
//      solutions: List[Inventory]
//  ): List[Inventory] =
//    if (queue.isEmpty) {
//      solutions
//    } else {
//      val e = queue.dequeue()
//
//      val (newSolutions, newMaxTime) = if (e.geode.deposit >= maxGeode) {
//        (e +: solutions, e.elapsed)
//      } else if (e.elapsed < maxTime && e.elapsed < 24) {
//        queue.enqueue(e.genChoices(): _*)
//        (solutions, maxTime)
//      } else {
//        // ignore, its too far back
//        (solutions, maxTime)
//      }
//
//      fullBfs(queue, newMaxTime, maxGeode, newSolutions)
//    }

  @tailrec
  def bfs(
      queue: mutable.PriorityQueue[Inventory],
      maxTime: Int,
      maxGeode: Int,
      solutions: List[Inventory],
      exploredCount: Int
  ): List[Inventory] =
    if (queue.isEmpty) {
      solutions
    } else {
      val e = queue.dequeue()

      val (newSolutions, newMaxTime) = if (e.geode.deposit >= maxGeode) {
        println(s"New solution ${e}")
        (e +: solutions, e.elapsed)
      } else if (e.elapsed < maxTime && e.elapsed < 24) {
        queue.enqueue(e.genChoices(): _*)
        (solutions, maxTime)
      } else {
        // ignore, its too far back
        (solutions, maxTime)
      }

      bfs(queue, newMaxTime, maxGeode, newSolutions, exploredCount + 1)
    }

  def parseBlueprint(s: String): Blueprint = {
    val oreRe      = "Blueprint (\\d+): .*Each ore robot costs (\\d+) ore.*".r
    val clayRe     = ".*Each clay robot costs (\\d+) ore.*".r
    val obsidianRe = ".*Each obsidian robot costs (\\d+) ore and (\\d+) clay.*".r
    val geodeRe    = ".*Each geode robot costs (\\d+) ore and (\\d+) obsidian.*".r

    val (id, ore)                   = s match { case oreRe(id, ore) => (id.toInt, ore.toInt) }
    val clay                        = s match { case clayRe(ore) => ore.toInt }
    val (obsidianOre, obsidianClay) = s match { case obsidianRe(ore, clay) => (ore.toInt, clay.toInt) }
    val (geodeOre, geodeObsidian)   = s match { case geodeRe(ore, obsidian) => (ore.toInt, obsidian.toInt) }

    Blueprint(id, ore, clay, obsidianOre, obsidianClay, geodeOre, geodeObsidian)
  }

  def parse(input: List[String]) = {
    val blueprints = input.splitVertically("").map { l =>
      parseBlueprint(l.mkString(""))
    }
    blueprints
  }
}
