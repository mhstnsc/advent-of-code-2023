package attic

import common._

import scala.annotation.tailrec
import scala.collection.immutable.Set

object Day24_v2_Problem1 extends MainBaseBig(24) {

  def parse(inputFile: List[String]): Matrix[Char] =
    Matrix(inputFile.map(_.toVector).toVector)

  case class WindState(
      map: Matrix[Char],
      winds: Iterable[(Point, Point)]
  ) {

    private def wrap(pos: Point): Point =
      if (pos.l == 0) {
        pos.copy(l = map.numLines() - 2)
      } else if (pos.l == map.numLines() - 1) {
        pos.copy(l = 1)
      } else if (pos.c == 0) {
        pos.copy(c = map.numCols() - 2)
      } else if (pos.c == map.numCols() - 1) {
        pos.copy(c = 1)
      } else pos

    def moveWinds(): WindState = {
      val newWinds = winds.map { case (windPos, incr) =>
        val newWindPos = windPos.inc(incr)
        wrap(newWindPos) -> incr
      }

      val emptyMap = map.updated(Point(1, 1), Matrix.filled(map.numLines() - 2, map.numCols() - 2, '.'))
      val newMap = newWinds.groupBy(_._1).foldLeft(emptyMap) { case (acc, (pos, inc)) =>
        require(acc.isInside(pos), s"$pos -> ${acc.numLines()} ${acc.numCols()}")
        acc.updated(pos, if (inc.size > 1) inc.size.toString.head else incrementToSign(inc.head._2))
      }

      WindState(newMap, newWinds)
    }

    private def incrementToSign(inc: Point): Char =
      inc match {
        case Point(1, 0)  => 'v'
        case Point(-1, 0) => '^'
        case Point(0, 1)  => '>'
        case Point(0, -1) => '<'
      }
  }

  override def run(inputFile: List[String]): String = {
    val map = parse(inputFile)
    val winds = map
      .values()
      .map { case (c, point) =>
        c match {
          case '#' => None
          case '.' => None
          case '>' => Some(point -> Point(0, 1))
          case 'v' => Some(point -> Point(1, 0))
          case '<' => Some(point -> Point(0, -1))
          case '^' => Some(point -> Point(-1, 0))
        }
      }
      .flatten

    val windState = WindState(
      map,
      winds
    )

    val destination = Point(map.numLines() - 1, map.numCols() - 2)
    val start       = Point(0, 1)

    println(map.values().count { case (c, p) => c == '.' })

    val initialExplorationQueue = Vector(ExploreCandidate(GraphNode(start, 0), GraphNode(start, 0)))

    // perform initial render so we can move cursor back
    render(windState, Vector.empty, Vector.empty, 0)

    def buildPath(history: Map[GraphNode, HistoryItem], start: GraphNode, end: GraphNode): Vector[GraphNode] =
      if (start == end) {
        Vector(start)
      } else {
        buildPath(history, start, history(end).parent).appended(end)
      }

    // solve first to find the solution
    val result = {
      val renderer = (_: WindState, _: Vector[ExploreCandidate], _: Int) => ()
      bfs(windState, 0, initialExplorationQueue, Set.empty, destination, Map.empty, renderer)
    }
    val endGraphNode = result.keySet.find(n => n.p == destination).get
    val solutionPath = buildPath(result, GraphNode(start, 0), endGraphNode)

    // solve again and render the solution as well
    val renderer = (windState: WindState, explorationQueue: Vector[ExploreCandidate], t: Int) => {
      Thread.sleep(30)
      println(s"${Terminal.moveCursorToPosition(7, 1)}")
      render(windState, explorationQueue, solutionPath, t)
    }
    bfs(windState, 0, initialExplorationQueue, Set.empty, destination, Map.empty, renderer)

    solutionPath.size.toString
  }

  def findFreeSpotsAround(newMap: Matrix[Char], pos: Point): Seq[Point] = {
    val increments = List(
      Point(1, 0),
      Point(0, 1),
      Point(-1, 0),
      Point(0, -1),
      Point(0, 0)
    )
    increments.map(i => pos.inc(i)).filter(p => newMap.isInside(p)).filter(p => newMap(p) == '.')
  }

  case class GraphNode(
      p: Point,
      time: Int
  )
  case class HistoryItem(parent: GraphNode)
  case class ExploreCandidate(candidate: GraphNode, parent: GraphNode)

  def render(
      windState: WindState,
      explorationQueue: Vector[ExploreCandidate],
      solutionPath: Vector[GraphNode],
      t: Int
  ): Unit = {
    val updatedMap = explorationQueue.map(_.candidate.p).toSet.foldLeft(windState.map) { case (acc, p) =>
      acc.updated(p, 'o')
    }

    // add the solution to the map
    val withSolution = solutionPath.filter(c => c.time <= t).foldLeft(updatedMap) { case (map, node) =>
      map.updated(node.p, 'S')
    }

    val renderer = (v: Char, _: Point) =>
      v match {
        case '.' => s"${Terminal.ANSI_BLUE}$v${Terminal.ANSI_RESET}"
        case 'o' => s"${Terminal.ANSI_RED}$v${Terminal.ANSI_RESET}"
        case '#' => s"${Terminal.ANSI_WHITE}$v${Terminal.ANSI_RESET}"
        case 'S' => s"${Terminal.ANSI_YELLOW}o${Terminal.ANSI_RESET}"
        case _   => s"${Terminal.ANSI_BLUE}$v${Terminal.ANSI_RESET}"
      }

    println(withSolution.mkString("", renderer, alignColumns = false))
  }

  @tailrec
  def bfs(
      windState: WindState,
      t: Int,
      explorationQueue: Vector[ExploreCandidate],
      explorationSet: Set[GraphNode],
      end: Point,
      history: Map[GraphNode, HistoryItem],
      renderer: (WindState, Vector[ExploreCandidate], Int) => Unit
  ): Map[GraphNode, HistoryItem] = {
    renderer(windState, explorationQueue, t)

    if (explorationQueue.isEmpty) {
      history
    } else {
      val nextWindState = windState.moveWinds()
      val graph         = genAllPossibleMoves(windState, t)

      val expanded = explorationQueue.map { case ExploreCandidate(current, _) =>
        val children = graph.getOrElse(current, Seq.empty)
        val newNodes = children
          .map(v => ExploreCandidate(v, current))

        newNodes
      }.flatten.distinct

      val updatedHistory = expanded.foldLeft(history) { case (acc, e) =>
        acc.updated(e.candidate, HistoryItem(e.parent))
      }

      if (expanded.exists(v => v.candidate.p == end)) {
        updatedHistory
      } else {
        val updatedExplorationQueue = expanded
        val updatedExplorationSet   = explorationSet ++ expanded.map(_.candidate)
        bfs(nextWindState, t + 1, updatedExplorationQueue, updatedExplorationSet, end, updatedHistory, renderer)
      }
    }
  }

  def genAllPossibleMoves(windState: WindState, t: Int): Map[GraphNode, Seq[GraphNode]] = {
    val nextWindState = windState.moveWinds()

    val m1      = windState.map
    val t1      = t
    val m2      = nextWindState.map
    val t2      = t + 1
    val m1Nodes = m1.values().map { case (c, p) => if (c == '.') Some(p) else None }.flatten.toSet
    val m2Nodes =
      m1Nodes.map(p => p -> findFreeSpotsAround(m2, p)).toMap

    val graph = m2Nodes.map { case (p, children) =>
      GraphNode(p, t1) -> children.map(p => GraphNode(p, t2))
    }
    graph
  }

  def buildGraph(map: Matrix[Char]): Map[GraphNode, Seq[GraphNode]] = {
    val winds = map
      .values()
      .map { case (c, point) =>
        c match {
          case '#' => None
          case '.' => None
          case '>' => Some(point -> Point(0, 1))
          case 'v' => Some(point -> Point(1, 0))
          case '<' => Some(point -> Point(0, -1))
          case '^' => Some(point -> Point(-1, 0))
        }
      }
      .flatten
    val allMaps = (1 to (map.numCols() - 2) * (map.numLines() - 2) * 2).foldLeft((Vector(map), WindState(map, winds))) {
      case ((maps, windstate), _) =>
        val nextWindState = windstate.moveWinds()
        (maps.appended(nextWindState.map), nextWindState)
    }

    val subgraphs = allMaps._1.zipWithIndex.sliding(2, 1).map { children =>
      val m1      = children(0)._1
      val t1      = children(0)._2
      val m2      = children(1)._1
      val t2      = children(1)._2
      val m1Nodes = m1.values().map { case (c, p) => if (c == '.') Some(p) else None }.flatten.toSet
      val m2Nodes =
        m1Nodes.map(p => p -> findFreeSpotsAround(m2, p)).toMap

      val graph = m2Nodes.map { case (p, children) =>
        GraphNode(p, t1) -> children.map(p => GraphNode(p, t2))
      }
      graph
    }

    subgraphs.foldLeft(Map.empty[GraphNode, Seq[GraphNode]]) { case (acc, m) =>
      acc ++ m
    }
  }
}
