import common.InputSyntax.InputSyntax
import common._

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable

object Day19_Problem1_v2 extends MainBaseSmall(19) {

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

  import scala.collection.parallel.CollectionConverters._

  override def run(inputFile: List[String]): String = {
    val blueprints = parse(inputFile)

    val solutions = blueprints.map { b =>
      (b, solve(b))
    }

    solutions.map { case (b, geodes) => b.id * geodes }.sum.toString
  }

  def solve(b: Blueprint): Int = {
    val maxTime = 24
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

    val allSolutions = (1 to maxTime).foldLeft(Map(RobotConfig(initialInventory) -> Vector(initialInventory))) {
      case (acc, t) =>
        val r = solveForTime(acc)
        println(s"${t} Sizes: ${acc.size}/${acc.map(_._2.size).sum} -> ${r.size}/${r.map(_._2.size).sum}")
        r
    }

    println(s"solving ${b} ... done")
    allSolutions.values.flatten.map(i => i.geode.deposit).max
  }

  case class RobotConfig(
      ore: Int,
      clay: Int,
      obs: Int,
      geo: Int
  )

  object RobotConfig {
    def apply(i: Inventory): RobotConfig =
      RobotConfig(i.ore.robots, i.clay.robots, i.obsidian.robots, i.geode.robots)
  }

  def solveForTime(knownConfigs: Map[RobotConfig, Vector[Inventory]]): Map[RobotConfig, Vector[Inventory]] =
    knownConfigs.map { case (robotConfig, i) =>
      i.map(_.genChoices())
    }.flatten.flatten
      .map(i => (RobotConfig(i), i))
      .groupBy(_._1)
      .view
      .mapValues { l =>
        implicit val o: Ordering[Inventory] = (a: Inventory, b: Inventory) =>
          if (
            a.ore.deposit <= b.ore.deposit && a.clay.deposit <= b.clay.deposit &&
            a.obsidian.deposit <= b.obsidian.deposit &&
            a.geode.deposit <= b.geode.deposit
          ) -1
          else 0
        l.map(_._2).toList.sorted(o.reverse).foldLeft(Vector.empty[Inventory]) { case (acc, i) =>
          acc.headOption.map { biggerValue =>
            if (o.compare(i, biggerValue) < 0) {
              acc
            } else {
              i +: acc
            }
          }.getOrElse(Vector(i))
        }
      }
      .toMap

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
