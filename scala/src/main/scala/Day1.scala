import common.MainBase
import common.InputSyntax._

object Day1 extends MainBase(1) {
  override def run(inputFile: List[String]): Unit = {
    val result = inputFile
      .splitVertically("")
      .map(_.map(_.toInt).sum)
      .sortWith((a, b) => a > b)
      .take(3)
      .sum
    println(result)
  }
}
