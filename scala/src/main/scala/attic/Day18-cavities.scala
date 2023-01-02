package attic

import attic.Day18._
import common.MainBaseBig

import scala.annotation.tailrec

object Day18 {
  val day = 18

  def parse(inputFile: List[String]): List[Coords] =
    inputFile.map { s =>
      val cube = s.split(",").map(_.toInt)
      Coords(cube(0), cube(1), cube(2))
    }

  def neighbours(c: Coords): Array[Coords] =
    Array(
      c.copy(c.x - 1, c.y, c.z),
      c.copy(c.x + 1, c.y, c.z),
      c.copy(c.x, c.y - 1, c.z),
      c.copy(c.x, c.y + 1, c.z),
      c.copy(c.x, c.y, c.z - 1),
      c.copy(c.x, c.y, c.z + 1)
    )

  @tailrec
  def expand(
      c: Coords,
      increment: Coords,
      cubeSet: Set[Coords],
      maxSpawn: Int,
      accumulated: List[Coords]
  ): Option[List[Coords]] = {
    val newC = Coords(c.x + increment.x, c.y + increment.y, c.z + increment.z)
    if (maxSpawn == 0) {
      None
    } else if (cubeSet.contains(newC)) {
      Some(accumulated)
    } else {
      expand(newC, increment, cubeSet, maxSpawn - 1, newC +: accumulated)
    }
  }

  def flood(start: Coords, candidates: Set[Coords], acc: Set[Coords]): Set[Coords] = {
    val newCavity            = acc + start
    val connectedNeightbours = neighbours(start).filter(n => candidates.contains(n) && !newCavity.contains(n))

    connectedNeightbours.foldLeft(newCavity) { case (acc, n) =>
      flood(n, candidates, acc)
    }
  }

  @tailrec
  def keepCavities(
      candidates: Set[Coords],
      cavities: Set[Coords],
      suroundings: Set[Coords],
      directions: List[(Coords, Int)]
  ): Set[Coords] =
    if (candidates.isEmpty) {
      cavities
    } else {
      val one    = candidates.head
      val cavity = flood(one, candidates - one, Set.empty)

      val isCavity = !cavity.exists { c =>
        val expansions = directions.map { case (increment, spawn) =>
          expand(c, increment, suroundings, spawn, Nil)
        }
        expansions.exists(_.isEmpty)
      }

      if (isCavity) {
        keepCavities(candidates.diff(cavity), cavities.union(cavity), suroundings, directions)
      } else {
        keepCavities(candidates.diff(cavity), cavities, suroundings, directions)
      }
    }

  case class Coords(x: Int, y: Int, z: Int)
}

object Day18_Problem2 extends MainBaseBig(day) {
  override def run(inputFile: List[String]): String = {
    val cubes   = parse(inputFile)
    val cubeSet = cubes.toSet

    val spawnX = cubes.map(_.x).max - cubes.map(_.x).min + 4
    val spawnY = cubes.map(_.y).max - cubes.map(_.y).min + 4
    val spawnZ = cubes.map(_.z).max - cubes.map(_.z).min + 4

    val directions = List(
      (Coords(1, 0, 0), spawnX),
      (Coords(-1, 0, 0), spawnX),
      (Coords(0, 1, 0), spawnY),
      (Coords(0, -1, 0), spawnY),
      (Coords(0, 0, -1), spawnZ),
      (Coords(0, 0, 1), spawnZ)
    )

    // expand all cube things to detect potential cavity cubes
    val cavityCandidate = cubes.map { c =>
      directions.map { case (increment, spawn) => expand(c, increment, cubeSet, spawn, Nil).getOrElse(Nil) }.flatten
    }.flatten.toSet

    // for the potential cavity cubes then connect the cubes which are side by side
    // and eliminate the blobs that have at least one expansion going to nowhere (not a cavity)
    val cavities = keepCavities(cavityCandidate, Set.empty, cubeSet, directions)
    require(cubeSet.intersect(cavities).isEmpty)

    // compute how many faces the cubes have exposed to the outside (in the check include also cavities
    val all = cubeSet ++ cavities
    val computed = cubes.map { c =>
      val ns = neighbours(c).filter(v => all.contains(v)).toList
      6 - ns.size
    }
    computed.sum.toString
  }
}
