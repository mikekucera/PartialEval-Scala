package pe.flowchart

import pe.flowchart.FlowChartSyntax._
import scala.annotation.tailrec

object FlowChartInterpreter {
  
  type Environment = Map[String, Value]
  type Tails = Map[String, List[Line]]
  
  sealed abstract trait Value {
    def string:String = throw new EvaluationException("expected a string")
    def list:List[String] = throw new EvaluationException("expected a list")
  }
  case class StringValue(override val string:String) extends Value 
  case class ListValue(override val list:List[String]) extends Value
  object ListValue {
    def apply(vals:String*): ListValue = ListValue(vals.toList)
  }
  
  class EvaluationException(message: String) extends RuntimeException(message)
  
  
  private def unaryOp(op: UnaryOp, value: Value): Value = {
    val xs = value.list
    op match {
      case Head => StringValue(xs.head)
      case Tail => ListValue(xs.tail)
      case FirstSym => StringValue(if(xs.isEmpty) "no-sym" else xs.head)
    }
  }
  
  private def binaryOp(op: BinaryOp, v1: Value, v2: Value): Value = op match {
    case Cons => ListValue(v1.string :: v2.list)
    case Split => ListValue(v2.string.split(v1.string).toList)
    case NewTail => ListValue(v2.list.dropWhile(!_.startsWith(v1.string)))
  }
  
  def eval(expr: Expression)(implicit env: Environment): Value = expr match {
    case Lst(l) => ListValue(l)
    case Str(s) => StringValue(s)
    case Unary(op, e) => unaryOp(op, eval(e))
    case Binary(op, e1, e2) => binaryOp(op, eval(e1), eval(e2))
    case Var(v) => env.get(v) getOrElse (throw new EvaluationException(s"undefined var '$v'"))
  }
  
  @tailrec
  private def run(programTail: List[Line])(implicit tails: Tails, env: Environment): Value = programTail match {
    case Nil => StringValue("")
    case Line(_, command) :: rest => command match {
      case Goto(label) => run(tails(label))
      case Return(expr) => eval(expr)
      case Assign(v, e) => run(rest)(tails, env + (v -> eval(e)))
      case IfGotoElse(e1,e2,l1,l2) => if(eval(e1) == eval(e2)) run(tails(l1)) else run(tails(l2))
    }
  }
  
  def programTails(program: Program): Tails = {
    def findTails(rest:List[Line], tails:Tails): Tails = rest match {
      case Nil => tails
      case l :: ls => findTails(ls, if(l.label.isEmpty) tails else tails + (l.label -> rest))
    }
    findTails(program.lines, Map())
  }
  
  def runProgram(program: Program, input: List[Value]): Value = program match {
    case Program(Read(names), lines) => {
      if(input.length < names.length) throw new EvaluationException("not enough input parameters")
      val env = Map() ++ (names zip input)
      val tails = programTails(program)
      run(lines)(tails, env)
    }
  }
}