package pe.schemezero

import pe.schemezero.SchemeZeroSyntax._

object SchemeZeroInterpreter {
  
  sealed abstract trait Value
  case class StringValue(val string:String) extends Value 
  case class ListValue(val list:List[Value]) extends Value
  
  
  def evalExpr(expr: Expression)(implicit mapping: Map[String,Value], program: Program): Option[Value] = expr match {
    case Var(name) => mapping.get(name)
    case Car(expr) => for(ListValue(x::xs) <- evalExpr(expr)) yield x
    case Cdr(expr) => for(ListValue(x::xs) <- evalExpr(expr)) yield ListValue(xs)
    case Cons(head, tail) => for {
      h <- evalExpr(head)
      ListValue(t) <- evalExpr(tail)
    } yield ListValue(h :: t)
    case If(cond, left, right) => for {
      bool <- evalRelation(cond)
      result <- if(bool) evalExpr(left) else evalExpr(right)
    } yield result
    case Call(name, args) => {
      val argValues = args.map(evalExpr)
      if(argValues.exists(_.isEmpty)) None
      else for {
        equation <- program.equations.find(_.name == name)
        result <- evalEquation(equation, argValues.flatten, program)
      } yield result
    }
  }
  
  def evalRelation(relation: Relation)(implicit mapping: Map[String,Value], program: Program): Option[Boolean] = relation match {
    case Equals(left, right) => for {
      left <- evalExpr(left)
      right <- evalExpr(right)
    } yield left == right
    case IsEmpty(expr) => for {
      ListValue(list) <- evalExpr(expr)
    } yield list.isEmpty
  }
  
  def findEquation(name: String, program: Program): Option[Equation] = 
    program.equations.find(_.name == name)
  
  def evalEquation(equation: Equation, args: List[Value], program: Program): Option[Value] = {
    val mapping = Map() ++ equation.parameters.zip(args)
    evalExpr(equation.body)(mapping, program)
  }
  
  def runProgram(program: Program, input: List[Value]): Option[Value] = program match {
    case Program(Nil) => None
    case Program(main::rest) => evalEquation(main, input, program)
  }
  
}