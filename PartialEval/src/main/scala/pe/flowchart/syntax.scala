package pe.flowchart

/*
    <label>      -> string
    <var>        -> string
    <op1>        -> hd | tl 
    <op2>        -> cons  
    <value>      -> empty list | string | list
    <constant>   -> quote <value>
    <expr>       -> <constant> | <var> | <op1> <expr> | <op2> <expr> <expr>
    <jump>       -> goto <label> | return <expr> | if <expr> = <expr> goto <label> else <label>
    <assignment> -> <var> := <expr>
    <basicblock> -> <label> : <assignment>* [<jump>]
    <program>    -> read <var>* ; <basicblock>+
*/

/**
 * Defines the abstract syntax for the flow chart language
 * and provides methods for pretty printing.
 */
object FlowChartSyntax {

  sealed trait UnaryOp
  case object Head extends UnaryOp
  case object Tail extends UnaryOp
  case object FirstSym extends UnaryOp
  
  sealed trait BinaryOp
  case object Cons extends BinaryOp
  case object NewTail extends BinaryOp
  case object Split extends BinaryOp
  
  sealed trait Expression
  case class Str(value: String) extends Expression
  case class Lst(members: List[String]) extends Expression
  case class Var(name: String) extends Expression
  case class Unary(op:UnaryOp, expr: Expression) extends Expression
  case class Binary(op:BinaryOp, left: Expression, right: Expression) extends Expression
  
  sealed trait Command
  case class Goto(label:String) extends Command
  case class Return(expr: Expression) extends Command
  case class IfGotoElse(e1: Expression, e2: Expression, goto: String, els: String) extends Command
  case class Assign(name: String, expr: Expression) extends Command
  
  case class Read(names: List[String])
  case class Line(label: String = "", command: Command)
  case class Program(read:Read, lines:List[Line])

  
  
  def pretty(program: Program): String =
    program.lines.map(prettyLine).mkString("\n")
  
  def pretty(expr:Expression): String = expr match {
    case Str(value) => "'" + value
    case Lst(members) => members.mkString("'(", ",", ")")
    case Var(name) => name
    case Unary(Head, expr) => s"hd(${pretty(expr)})"
    case Unary(Tail, expr) => s"tl(${pretty(expr)})"
    case Unary(FirstSym, expr) => s"first_sym(${pretty(expr)})"
    case Binary(Cons, left, right) => s"cons(${pretty(left)},${pretty(right)})"
    case Binary(NewTail, left, right) => s"new_tail(${pretty(left)},${pretty(right)})"
    case Binary(Split, left, right) => s"split(${pretty(left)},${pretty(right)})"
  }
  
  def prettyLine(line:Line): String = {
    val l = line.label
    line.command match {
      case Goto(label) =>  f"$l%-10s: goto $label;"
      case Return(expr) => f"$l%-10s: return ${pretty(expr)});"
      case IfGotoElse(e1, e2, goto, els) => f"$l%-10s: if ${pretty(e1)} = ${pretty(e2)} then $goto else $els"
      case Assign(name, expr) => f"$l%-10s: $name := ${pretty(expr)};";
    }
  }
  
}