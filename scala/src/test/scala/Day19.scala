import Day19_Problem1._
import attic.Day16_Problem1_Branch_Bound.Graph
import attic.Day16_Problem2_Branch_Bound.Branch
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.TreeSet
class Day19 extends AnyFunSuite with Matchers {

  test("tick") {
    val i = Inventory(
      ore = Resource(2, 2),
      clay = Resource(10, 10),
      obsidian = Resource(100, 100),
      geode = Resource(1000, 1000),
      blueprint = Blueprint(
        id = 1,
        ore_OreCost = 1,
        clay_OreCost = 10,
        obs_OreCost = 10,
        obs_ClayCost = 10,
        geo_OreCost = 10,
        geo_ObsCost = 10
      ),
      elapsed = 0,
      firstGeoTime = 0
    )

    i.tick shouldBe i.copy(
      ore = Resource(4, 2),
      clay = Resource(20, 10),
      obsidian = Resource(200, 100),
      geode = Resource(2000, 1000),
      elapsed = 1
    )
  }

  test("inventory choices") {
    val i = Inventory(
      ore = Resource(2, 2),
      clay = Resource(10, 10),
      obsidian = Resource(100, 100),
      geode = Resource(1000, 1000),
      blueprint = Blueprint(
        ore_OreCost = 2,
        clay_OreCost = 2,
        obs_OreCost = 2,
        obs_ClayCost = 10,
        geo_OreCost = 2,
        geo_ObsCost = 100
      ),
      elapsed = 0,
      firstGeoTime = 0
    )

    i.genChoices() should contain theSameElementsAs (
      List(
        i.copy(
          ore = Resource(2, 3),
          clay = Resource(20, 10),
          obsidian = Resource(200, 100),
          geode = Resource(2000, 1000),
          elapsed = 1
        ),
        i.copy(
          ore = Resource(2, 2),
          clay = Resource(20, 11),
          obsidian = Resource(200, 100),
          geode = Resource(2000, 1000),
          elapsed = 1
        ),
        i.copy(
          ore = Resource(2, 2),
          clay = Resource(10, 10),
          obsidian = Resource(200, 101),
          geode = Resource(2000, 1000),
          elapsed = 1
        ),
        i.copy(
          ore = Resource(2, 2),
          clay = Resource(20, 10),
          obsidian = Resource(100, 100),
          geode = Resource(2000, 1001),
          elapsed = 1
        ),
        i.tick
      )
    )

  }

  test("branch - appended") {}

  test("producingValves") {}
}
