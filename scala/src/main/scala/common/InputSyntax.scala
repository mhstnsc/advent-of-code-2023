package common

object InputSyntax {
  implicit class InputSyntax(input: List[String]) {
    def splitVertically(separatorLine: String): List[List[String]] = {
      val (r, first) = input.foldRight((List.empty[List[String]], List.empty[String])) { case (v, (r, c)) =>
        if (v == separatorLine) {
          (c +: r, List.empty[String])
        } else
          (r, v +: c)
      }
      (first +: r).filter(_.nonEmpty)
    }
  }
}
