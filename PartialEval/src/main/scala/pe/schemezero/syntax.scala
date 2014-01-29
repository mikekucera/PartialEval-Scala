package pe.schemezero


object SchemeZeroSyntax {
  
  sealed trait Relation
  case class Equals(left: Expression, right: Expression) extends Relation
  case class IsEmpty(expr: Expression) extends Relation
  
  sealed trait Expression
  case class Var(name: String) extends Expression
  case class If(cond: Relation, left: Expression, right: Expression) extends Expression
  case class Call(name: String, arguments: List[Expression]) extends Expression
  case class Car(expr: Expression) extends Expression
  case class Cdr(expr: Expression) extends Expression
  case class Cons(head: Expression, tail: Expression) extends Expression
  
  
  case class Equation(name: String, parameters: List[String], body: Expression)
  case class Program(equations: List[Equation])
  
}