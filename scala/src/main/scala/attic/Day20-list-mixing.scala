package attic

import common._

import scala.collection.Searching
import scala.collection.immutable.TreeMap

object Day20_Problem1 extends MainBaseBig(20) {

  def parse(input: List[String]): Seq[BigInt] =
    input.map(v => BigInt(v.toInt))

  def computeDiffDistance(currentIdx: Int, displacement: BigInt, size: Int): Int = {
    if (currentIdx + displacement <= 0) {
      BigInt(size - 1) + (BigInt(currentIdx) + displacement) % BigInt(size)
    } else {
      (BigInt(currentIdx) + displacement) % BigInt(size)
    }
  }.toInt

  override def run(inputFile: List[String]): String = {
    val message = parse(inputFile).toVector

    case class State(
        idxToKey: Map[Int, BigDecimal],
        keyToIdx: TreeMap[BigDecimal, Int]
    )
    val initialState = State(
      idxToKey = Map(message.indices.map { i =>
        i -> BigDecimal(i)
      }: _*),
      keyToIdx = TreeMap(message.indices.map(i => BigDecimal(i) -> i): _*)
    )

    val bigDecimalOrdering                             = implicitly[Ordering[BigDecimal]]
    implicit val ordering: Ordering[(BigDecimal, Int)] = (a, b) => bigDecimalOrdering.compare(a._1, b._1)

    def reconstruct(state: State): Seq[BigInt] =
      state.keyToIdx.toIndexedSeq.map { case (_, origIdx) =>
        message(origIdx)
      }

    val finalState = message.zipWithIndex.foldLeft(initialState) { case (acc, (v, originalIdx)) =>
      println(
        v + ":" +
          reconstruct(acc)
            .mkString(" ")
      )

      val key = acc.idxToKey(originalIdx)

      if (v != 0) {
        val keySeq = acc.keyToIdx.toIndexedSeq
        keySeq.search((key, originalIdx)) match {
          case Searching.Found(foundIndex) =>
            val newIdx = computeDiffDistance(foundIndex, v, message.size)
            val newKey = if (newIdx == message.length - 1) {
              keySeq(newIdx)._1 + 1
            } else {
              (keySeq(newIdx)._1 + keySeq(newIdx + 1)._1) / 2
            }

            State(
              acc.idxToKey
                .updated(originalIdx, newKey),
              acc.keyToIdx
                .removed(key)
                .updated(newKey, originalIdx)
            )
        }
      } else {
        acc
      }
    }

    val keySeq            = finalState.keyToIdx.toIndexedSeq
    val originalIdxOfZero = message.indexOf(0)
    val movedIdxOfZero    = keySeq.search((finalState.idxToKey(originalIdxOfZero), originalIdxOfZero)).insertionPoint

    def getValue(newPos: Int): BigInt =
      message(
        finalState.keyToIdx(
          keySeq(newPos % message.size)._1
        )
      )

    println(reconstruct(finalState).mkString(" "))

    val s = List(1000, 2000, 3000).map(i => getValue(movedIdxOfZero + i)).sum
    s.toString
  }
}
