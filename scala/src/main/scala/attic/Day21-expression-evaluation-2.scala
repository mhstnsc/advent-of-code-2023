package attic

import common._

object Day21_Problem2 extends MainBaseBig(21) {

  sealed trait Operation
  sealed trait BiOp extends Operation {
    val a: String
    val b: String
  }
  case class Value(v: BigDecimal)        extends Operation
  case class Mul(a: String, b: String)   extends BiOp
  case class Div(a: String, b: String)   extends BiOp
  case class Plus(a: String, b: String)  extends BiOp
  case class Minus(a: String, b: String) extends BiOp

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

  def evaluate(monkey: String, monkeys: Map[String, Monkey]): BigDecimal = {
    def deepEvaluate(monkey: Monkey): BigDecimal =
      monkey.op match {
        case Value(v)    => v
        case Mul(a, b)   => deepEvaluate(monkeys(a)) * deepEvaluate(monkeys(b))
        case Div(a, b)   => deepEvaluate(monkeys(a)) / deepEvaluate(monkeys(b))
        case Plus(a, b)  => deepEvaluate(monkeys(a)) + deepEvaluate(monkeys(b))
        case Minus(a, b) => deepEvaluate(monkeys(a)) - deepEvaluate(monkeys(b))
      }
    deepEvaluate(monkeys(monkey))
  }

  def valueOfHumn(monkeys: Map[String, Monkey]): BigDecimal = {
    // find the human and ancestors
    val parentMap = monkeys.map { case (name, monkeySpec) =>
      monkeySpec.op match {
        case op: BiOp => List(op.a -> name, op.b -> name)
        case Value(v) => Nil
      }
    }.flatten.toMap

    val goingDownPath: Seq[String] = {
      def buildRecursive(acc: Vector[String], monkey: String): Vector[String] =
        if (monkey == "root") acc.prepended("root")
        else buildRecursive(acc.prepended(monkey), parentMap(monkey))
      buildRecursive(Vector.empty, "humn")
    }

    def fancyEval(path: Seq[String], parentValue: BigDecimal): BigDecimal =
      path match {
        case current +: child +: _ =>
          monkeys(current).op match {
            case op: BiOp =>
              if (current == "root") {
                if (op.a == child) fancyEval(path.drop(1), evaluate(op.b, monkeys))
                else fancyEval(path.drop(1), evaluate(op.a, monkeys))
              } else {
                op match {
                  case Mul(a, b) =>
                    if (a == child) fancyEval(path.drop(1), parentValue / evaluate(b, monkeys))
                    else fancyEval(path.drop(1), parentValue / evaluate(a, monkeys))
                  case Div(a, b) =>
                    if (a == child) fancyEval(path.drop(1), parentValue * evaluate(b, monkeys))
                    else fancyEval(path.drop(1), evaluate(a, monkeys) / parentValue)
                  case Plus(a, b) =>
                    if (a == child) fancyEval(path.drop(1), parentValue - evaluate(b, monkeys))
                    else fancyEval(path.drop(1), parentValue - evaluate(a, monkeys))
                  case Minus(a, b) =>
                    if (a == child) fancyEval(path.drop(1), parentValue + evaluate(b, monkeys))
                    else fancyEval(path.drop(1), evaluate(a, monkeys) - parentValue)
                }
              }
          }
        case current +: Nil =>
          require(current == "humn")
          parentValue
      }

    fancyEval(goingDownPath, 0)
  }

  override def run(inputFile: List[String]): String = {
    val monkeys = parse(inputFile).map(v => (v.name, v)).toMap
    valueOfHumn(monkeys).toString()
  }
}
