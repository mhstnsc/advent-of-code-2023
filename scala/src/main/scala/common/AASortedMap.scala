package common
import common.AASortedMap.Node

trait SortedMap[K, V] extends Map[K, V] {

  def find(k: K): SortedMap.Iterator[K, V]
  def get(k: K): Option[V]
  def removed(k: K): SortedMap[K, V]
  def updated(k: K, v: V): SortedMap[K, V]
  def countBefore(k: K): Int
}

object SortedMap {
  trait Iterator[K, V] {
    def moveBy(diff: Int): Iterator[K, V]
    def key: K
    def value: V
    def previous: Iterator[K, V]
    def next: Iterator[K, V]
    def isValid: Boolean
  }
}

case class AAIterator[K,V](stack: List[Node[K,V]]) extends SortedMap.Iterator[K,V] {
  override def moveBy(diff: Int): SortedMap.Iterator[K, V] = ???
  override def key: K = {
    stack.head.k
  }
  override def value: V = {
stack.head.v
  }
  override def previous: SortedMap.Iterator[K, V] = ???
  override def next: SortedMap.Iterator[K, V] = ???
  override def isValid: Boolean = ???
}

case class AASortedMap[K: Ordering, V](root: Option[Node[K, V]] = None) extends SortedMap[K, V] {
  private val ordering = implicitly[Ordering[K]]

  override def find(k: K): SortedMap.Iterator[K, V] = {
//    def findImpl(maybeNode: Option[Node[K, V]]): SortedMap.Iterator[K, V] =
//    {
//      maybeNode.flatMap { n =>
//        val order = ordering.compare(k, n.k)
//        if (order < 0) {
//          findImpl(n.left)
//        } else if (order > 0) {
//          findImpl(n.right)
//        } else {
//          Some(n.v)
//        }
//      }
//    }
//    findImpl(root)
    ???
  }
  override def get(k: K): Option[V]                 = {
    def getImpl(maybeNode: Option[Node[K, V]]): Option[V] = {
      maybeNode.flatMap { n =>
        val order = ordering.compare(k, n.k)
        if (order < 0) {
          getImpl(n.left)
        } else if (order > 0) {
          getImpl(n.right)
        } else {
          Some(n.v)
        }
      }
    }
    getImpl(root)
  }

  override def removed(k: K): SortedMap[K, V] = {
    def removedImpl(maybeNode: Option[Node[K, V]]): Option[Node[K, V]] =
      maybeNode.flatMap { n =>
        val order = ordering.compare(k, n.k)
        val newNode = if (order < 0) {
          removedImpl(n.left).map(newLeft => n.copy(left = Some(newLeft)))
        } else if (order > 0) {
          removedImpl(n.right).map(newRight => n.copy(right = Some(newRight)))
        } else {
          None
        }
        updateCachedData(newNode)
      }
    new AASortedMap(removedImpl(root))
  }

  private def updateCachedData(node: Option[Node[K,V]]): Option[Node[K,V]] = {
    node.map(n =>
      n.copy(
        count = n.left.map(_.count).getOrElse(0) + n.right.map(_.count).getOrElse(0) + 1,
        level = 1 + math.max(n.left.map(_.level).getOrElse(0), n.right.map(_.level).getOrElse(0))
      )
    )
  }

  override def updated(k: K, v: V): SortedMap[K, V] = {
    def updatedImpl(maybeNode: Option[Node[K, V]]): Option[Node[K, V]] =
      maybeNode.flatMap { n =>
        val order = ordering.compare(k, n.k)
        val newNode = if (order < 0) {
          val maybeNewLeft = updatedImpl(n.left)
          maybeNewLeft.map(newLeft => n.copy(left = Some(newLeft)))
        } else if (order > 0) {
          updatedImpl(n.right).map(newRight => n.copy(right = Some(newRight)))
        } else {
          // equality so just update the value
          Some(n.copy(v = v))
        }
        updateCachedData(newNode)
      }.orElse(
        Some(Node(k = k, v = v, left = None, right = None, 0, 0))
      )
    AASortedMap(updatedImpl(root))
  }
  override def countBefore(k: K): Int                          = ???
  override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = ???
  override def iterator: Iterator[(K, V)]                      = ???
}

object AASortedMap {
  case class Node[K, V](
      k: K,
      v: V,
      left: Option[Node[K, V]],
      right: Option[Node[K, V]],
      count: Int,
      level: Int
  )
}
