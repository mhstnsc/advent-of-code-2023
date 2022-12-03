import Day3.{letterToPrio, ProblemBase}
import common.MainBase

object Day3 {
  def letterToPrio(c: Char) =
    if (c >= 'a' && c <= 'z') c - 'a' + 1
    else if (c >= 'A' && c <= 'Z') c - 'A' + 27
    else throw new Exception(s"wtf for ${c}")

  class ProblemBase(day: Int) extends MainBase(day) {

    override def run(inputFile: List[String]): String =
      inputFile.map { s =>
        val l          = s.toList
        val firstHalf  = l.take(l.size / 2).toSet
        val secondHalf = l.drop(l.size / 2).toSet
        val common     = firstHalf.intersect(secondHalf)
        common.map(letterToPrio).sum
      }.sum.toString
  }
}

object Day3Problem1 extends ProblemBase(3)

object Day3Problem2 extends ProblemBase(3) {

  override def run(inputFile: List[String]): String =
    inputFile
      .sliding(3, 3)
      .flatMap { groupInput =>
        groupInput
          .map(_.toSet)
          .reduce(_ intersect _)
          .map(letterToPrio)
      }
      .sum
      .toString
}
