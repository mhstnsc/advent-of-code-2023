package attic

import common._

import scala.collection.Searching
import scala.collection.immutable.TreeMap

object Day20_Problem2_v2 extends MainBaseSmall(20) {

  val multiplier = 811589153
  // val multiplier = 1

  def parse(input: List[String]): Seq[BigInt] =
    input.map(v => BigInt(v.toInt)).map(_ * multiplier)

  def computeDiffDistance(currentIdx: Int, displacement: BigInt, size: Int): Int = {
    val newIdx = (BigInt(currentIdx) + displacement) % (size - 1)
    if (newIdx < 0) {
      size - 1 + newIdx
    } else {
      newIdx
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

    val finalState =
      List.fill(10)(message.zipWithIndex).flatten.foldLeft(initialState) { case (acc, (v, originalIdx)) =>
        debugln(
          v + s": mod ${v % (message.size - 1)}: [${reconstruct(acc).mkString(" ")}]"
        )

        val key = acc.idxToKey(originalIdx)

        if (v != 0) {
          val searchResult           = acc.keyToIdx.toIndexedSeq.search((key, originalIdx))
          val idxToKeyWithoutCurrent = acc.keyToIdx.removed(key)
          searchResult match {
            case Searching.Found(foundIndex) =>
              val keySeq = idxToKeyWithoutCurrent.toIndexedSeq
              val newIdx = computeDiffDistance(foundIndex, v, message.size)
              val newKey = if (newIdx == 0) {
                keySeq(message.size - 2)._1 + 1
              } else {
                (keySeq(newIdx - 1)._1 + keySeq(newIdx)._1) / 2
              }

              println(s"computed new idx: ${newIdx}")
              if (newIdx == foundIndex) {
                acc
              } else {
                State(
                  acc.idxToKey
                    .updated(originalIdx, newKey),
                  idxToKeyWithoutCurrent.updated(newKey, originalIdx)
                )
              }

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
