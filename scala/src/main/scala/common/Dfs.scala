//package common
//
//object Dfs {
//  trait Node[NODEID] {
//    def children(): List[Node[NODEID]]
//    def id(): NODEID
//  }
//
//  def dfs[MYNODE <: Node[NODEID], NODEID, STATE](
//      root: MYNODE,
//      state: STATE,
//      newState: (STATE, MYNODE) => STATE
//  ): STATE = {
//
//    case class InternalState(visited: Set[NODEID] = Set(), explorationQueue: List[NODEID] = Nil)
//
//    def internalDs(current: MYNODE, state: STATE, internalState: InternalState): STATE = {
//      internalState.explorationQueue.headOption
//        .map {
//          e =>
//
//        }.getOrElse(state)
//      )
//    }
//
//    val children             = root.children().filterNot(c => visited.contains(c.id())).asInstanceOf[List[MYNODE]]
//
//    val myState = newState(state, root)
//
//    children.foldLeft(state) { case (existingState, c) =>
//      dfs(c, myState, newState)
//    }
//  }
//}
