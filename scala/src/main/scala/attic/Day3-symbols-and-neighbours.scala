package attic

import common.MainBase

private abstract class Day3Base extends MainBase(3) {
  def findSymbol(inputFile: List[String], lineIdx: Int, colStart: Int, colEnd: Int): Boolean = {
    val coords = {
      (colStart - 1 to colEnd).map(colIdx => (lineIdx - 1, colIdx)) ++ List((lineIdx, colStart - 1)) ++ List(
        (lineIdx, colEnd)
      ) ++ (colStart - 1 to colEnd).map(colIdx => (lineIdx + 1, colIdx))
    }.filter { case (l, c) =>
      l >= 0 && l < inputFile.length && c >= 0 && c < inputFile(0).length
    }

    val exists = coords.exists { case (l, c) =>
      val ch     = inputFile(l)(c)
      val exists = ch != '.' && (ch < '0' || ch > '9')
      exists
    }
    exists
  }
}

private object Day3Problem1 extends Day3Base {
  override def run(inputFile: List[String]): String = {
    val inputWithIndex = inputFile.zipWithIndex
    val numberRe       = "[0-9]+".r

    inputWithIndex.map { case (line, lineIdx) =>
      numberRe
        .findAllMatchIn(line)
        .filter { m =>
          findSymbol(inputFile, lineIdx, m.start, m.end)
        }
        .map(m => m.group(0).toInt)
    }.flatten.sum.toString
  }
}

private object Day3Problem2 extends Day3Base {
  private def extractNumbers(inputFile: List[String], l: Int, c: Int): List[BigInt] = {
    val numberRe = "[0-9]+".r

    {
      for {
        li <- l - 1 to l + 1
        if li >= 0 && li < inputFile.length
        numbers = numberRe
          .findAllMatchIn(inputFile(li))
          .toList
          .filter(m => (m.start <= c && m.end >= c) || m.end == c || m.start == c + 1)
          .map(m => BigInt(m.group(0).toInt))
      } yield numbers
    }.flatten.toList
  }

  override def run(inputFile: List[String]): String = {
    val products = for {
      l <- 0 until inputFile.length
      c <- 0 until inputFile(0).length
      if inputFile(l)(c) == '*'
      numbers = extractNumbers(inputFile, l, c)
      if numbers.length == 2
      p = numbers(0) * numbers(1)
    } yield p

    products.sum.toString()
  }

}
