package attic

import common._

object Day21_Problem1 extends MainBaseBig(21) {

  sealed trait Operation
  case class Value(v: BigDecimal)        extends Operation
  case class Mul(a: String, b: String)   extends Operation
  case class Div(a: String, b: String)   extends Operation
  case class Plus(a: String, b: String)  extends Operation
  case class Minus(a: String, b: String) extends Operation

  case class Monkey(
      name: String,
      op: Operation
  )

  def parseMonkey(s: String): Monkey = {
    val simpleRe = "([a-z]+): (\\d+)".r
    val opRe     = "([a-z]+): ([a-z]+) ([*/+-]) ([a-z]+)".r

    s match {
      case simpleRe(name, value)     => Monkey(name, Value(BigDecimal(value)))
      case opRe(name, op1, "*", op2) => Monkey(name, Mul(op1, op2))
      case opRe(name, op1, "/", op2) => Monkey(name, Div(op1, op2))
      case opRe(name, op1, "+", op2) => Monkey(name, Plus(op1, op2))
      case opRe(name, op1, "-", op2) => Monkey(name, Minus(op1, op2))
    }
  }
  def parse(input: List[String]): List[Monkey] =
    input.map(parseMonkey)

  def evaluate(monkey: Monkey, monkeys: Map[String, Monkey]): BigDecimal = {
    def deepEvaluate(monkey: Monkey): BigDecimal =
      monkey.op match {
        case Value(v)    => v
        case Mul(a, b)   => deepEvaluate(monkeys(a)) * deepEvaluate(monkeys(b))
        case Div(a, b)   => deepEvaluate(monkeys(a)) / deepEvaluate(monkeys(b))
        case Plus(a, b)  => deepEvaluate(monkeys(a)) + deepEvaluate(monkeys(b))
        case Minus(a, b) => deepEvaluate(monkeys(a)) - deepEvaluate(monkeys(b))
      }
    deepEvaluate(monkey)
  }

  override def run(inputFile: List[String]): String = {
    println("exeucting")
    val monkeys = parse(inputFile).map(v => (v.name, v)).toMap
    evaluate(monkeys("root"), monkeys).toString()
  }
}
