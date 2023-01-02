package common
import scala.util.Random

object Terminal {
  val ANSI_RESET          = "\u001B[0m"
  val ANSI_BLACK          = "\u001B[30m"
  val ANSI_RED            = "\u001B[31m"
  val ANSI_GREEN          = "\u001B[32m"
  val ANSI_YELLOW         = "\u001B[33m"
  val ANSI_BLUE           = "\u001B[34m"
  val ANSI_PURPLE         = "\u001B[35m"
  val ANSI_CYAN           = "\u001B[36m"
  val ANSI_WHITE          = "\u001B[37m"
  val ANSI_BLACK_ON_WHITE = "\u001B[30;40m"

  val ANSI_CLEAR_SCREEN = "\u001B[2J"

  // 1 based values (1,1) is top left
  def moveCursorToPosition(line: Int, col: Int): String =
    s"\u001B[$line;${col}H"

  def moveCursorUp(amount: Int): String =
    s"\u001B[${amount}A"

  val colors = Array(ANSI_RED, ANSI_GREEN, ANSI_YELLOW, ANSI_BLUE, ANSI_PURPLE, ANSI_CYAN)

  def getColor(i: Int): String =
    colors(i % colors.length)

  def getRandomColor(i: Int): String = {
    val v = new Random(i).nextInt(colors.length)
    colors(v)
  }

  def getRandom256Color(seed: Int): String = {
    val i = new Random(seed).nextInt(256)
    s"\u001B[38;5;${i}m"
  }
}
