package attic

import attic.Day13.{isSmaller, mkstring, unsafeBuildTree}
import common.InputSyntax.InputSyntax
import common.MainBase

object Day13 {
  sealed trait Node
  case class Leaf(v: Long)           extends Node
  case class Parent(l: Vector[Node]) extends Node

  def mkstring(node: Node): String =
    node match {
      case Leaf(v)   => v.toString
      case Parent(l) => '[' + l.map(mkstring).mkString(",") + ']'
    }

  def isNumber(c: Char): Boolean =
    c >= '0' && c <= '9'

  def accumulateUntilNone(s: String): (String, Vector[Node]) = {
    val (remaining1, node1) = buildTree(s)
    if (node1.isEmpty) {
      (remaining1, Vector.empty)
    } else {
      val (rest, otherSiblings) = accumulateUntilNone(remaining1)
      (rest, Vector(node1.get) ++ otherSiblings)
    }
  }

  def buildTree(s: String): (String, Option[Node]) =
    if (s.isEmpty) {
      ("", None)
    } else if (s(0) == '[') {
      val (rest, node2) = accumulateUntilNone(s.drop(1))
      (rest, Some(Parent(node2)))
    } else if (s(0) == ']') {
      (s.drop(1), None)
    } else {
      if (s(0) >= '0' && s(0) <= '9') {
        val indexOfNonNumber = s.indexWhere(c => !isNumber(c))
        val safeIndex        = Option.when(indexOfNonNumber == -1)(s.length).getOrElse(indexOfNonNumber)
        val numberString     = s.slice(0, safeIndex)
        val leaf             = Leaf(numberString.toLong)
        (s.slice(safeIndex, Int.MaxValue), Some(leaf))
      } else {
        if (s(0) == ',') {
          buildTree(s.drop(1))
        } else {
          throw new Exception("unrecognized character")
        }
      }
    }

  def unsafeBuildTree(s: String): Node =
    buildTree(s)._2.get

  def isSmaller(v1: Vector[Node], v2: Vector[Node]): Option[Boolean] =
    (v1, v2) match {
      case (IndexedSeq(), IndexedSeq()) => None
      case (IndexedSeq(), _ +: _)       => Some(true)
      case (_ +: _, IndexedSeq())       => Some(false)
      case (l1 +: rest1, l2 +: rest2) =>
        isSmaller(l1, l2).orElse(isSmaller(rest1, rest2))
    }

  def isSmaller(t1: Node, t2: Node): Option[Boolean] =
    (t1, t2) match {
      case (Leaf(v1), Leaf(v2)) => Option.when(v1 < v2)(true).orElse(Option.when(v1 > v2)(false))
      case (Leaf(v1), Parent(l)) =>
        l match {
          case l +: rest    => isSmaller(t1, l).orElse(isSmaller(Vector.empty, rest))
          case IndexedSeq() => Option(false)
        }
      case (Parent(_), Leaf(v1))    => isSmaller(t2, t1).map(!_)
      case (Parent(l1), Parent(l2)) => isSmaller(l1, l2)
    }
}

object Day13Problem1 extends MainBase(13) {

  override def run(inputFile: List[String]): String = {
    val treeGroups = inputFile.splitVertically("").map(group => group.map(unsafeBuildTree))

    val printed = treeGroups.map(l => l.map(mkstring)).mkString("\n")
    println(printed)

    val filtered = treeGroups.zipWithIndex.filter { case (group, i) => isSmaller(group(0), group(1)).getOrElse(false) }

    val indices = filtered.map(_._2 + 1)
    println(indices.mkString(","))
    indices.sum.toString
  }
}

object Day13Problem2 extends MainBase(13) {
  override def run(inputFile: List[String]): String = {
    val divider1 = "[[2]]"
    val divider2 = "[[6]]"
    val treeGroups =
      (divider1 +: divider2 +: inputFile).splitVertically("").flatMap(group => group.map(unsafeBuildTree))

    val sortedGroups = treeGroups.sortWith((v1, v2) => isSmaller(v1, v2).getOrElse(false))

    val index1 = sortedGroups.indexWhere(t => mkstring(t) == divider1)
    val index2 = sortedGroups.indexWhere(t => mkstring(t) == divider2)

    ((index1 + 1) * (index2 + 1)).toString
  }
}
