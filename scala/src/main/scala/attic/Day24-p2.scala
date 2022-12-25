package attic

import common._

import scala.annotation.tailrec

object Day24_v2_Problem2 extends MainBaseBig(24) {

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

  @tailrec
  def bfs(
      graph: Map[GraphNode, Seq[GraphNode]],
      explorationQueue: Vector[ExploreCandidate],
      explorationSet: Set[GraphNode],
      end: Point,
      history: Map[GraphNode, HistoryItem]
  ): Map[GraphNode, HistoryItem] =
    if (explorationQueue.isEmpty) {
      history
    } else {
      val ExploreCandidate(current, parent) = explorationQueue.head
      val updatedHistory                    = history.updated(current, HistoryItem(parent))

      if (current.p == end) {
        updatedHistory
      } else {
        val children = graph.getOrElse(current, Seq.empty)

        val newNodes = children
          .filter(v => !updatedHistory.contains(v))
          .filter(p => !explorationSet.contains(p))
          .map(v => ExploreCandidate(v, current))

        val updatedExplorationQueue = (explorationQueue ++ newNodes.toVector).drop(1)
        val updatedExplorationSet   = explorationSet ++ newNodes.map(_.candidate)

        bfs(graph, updatedExplorationQueue, updatedExplorationSet, end, updatedHistory)
      }
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
    val allMaps = (1 to (map.numCols()-2) * (map.numLines()-2) * 3).foldLeft((Vector(map), WindState(map, winds))) {
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
        m1Nodes.map { p => p -> findFreeSpotsAround(m2, p) }.toMap

      val graph = m2Nodes.map { case (p, children) =>
        GraphNode(p, t1) -> children.map(p => GraphNode(p, t2))
      }
      graph
    }

    subgraphs.foldLeft(Map.empty[GraphNode, Seq[GraphNode]]) { case (acc, m) =>
      acc ++ m
    }
  }

  def buildPath(history: Map[GraphNode, HistoryItem], start: GraphNode, end: GraphNode): Vector[GraphNode] =
    if (start == end) {
      Vector(start)
    } else {
      buildPath(history, start, history(end).parent).appended(end)
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

    val destination = Point(map.numLines() - 1, map.numCols() - 2)
    val start       = Point(0, 1)

    val graph: Map[GraphNode, Seq[GraphNode]] = buildGraph(map)

    val hasEnding = graph.filter(_._2.exists(_.p == destination))

    println(map.values().count { case (c, p) => c == '.' })


    val forwardPath = {
      val initialExplorationQueue = Vector(ExploreCandidate(GraphNode(start, 0), GraphNode(start, 0)))
      val result                  = bfs(graph, initialExplorationQueue, Set.empty, destination, Map.empty)
      val endGraphNode = result.keySet.find { n => n.p == destination }.get
      buildPath(result, GraphNode(start, 0), endGraphNode)
    }

    println(s"Found ${forwardPath.size-1} computing forward trip")

    val backPath = {
      val endGraphNode = forwardPath.last
      val initialExplorationQueue = Vector(ExploreCandidate(endGraphNode, endGraphNode))
      val bfsResult = bfs(graph, initialExplorationQueue, Set.empty, start, Map.empty)

      val startGraphNode = bfsResult.keySet.find { n => n.p == start }.get
      buildPath(bfsResult, endGraphNode, startGraphNode)
    }

    println(s"Found ${backPath.size-1} computing back trip")

    val andForwardPath = {
      val endGraphNode = backPath.last
      val initialExplorationQueue = Vector(ExploreCandidate(endGraphNode, endGraphNode))
      val bfsResult = bfs(graph, initialExplorationQueue, Set.empty, destination, Map.empty)

      val startGraphNode = bfsResult.keySet.find { n => n.p == destination }.get
      buildPath(bfsResult, endGraphNode, startGraphNode)
    }

    println(s"Found ${andForwardPath.size-1} computing back trip")

    (forwardPath.size + backPath.size + andForwardPath.size-3).toString
  }
}
