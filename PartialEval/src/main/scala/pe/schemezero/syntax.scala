package pe.schemezero


object SchemeZeroSyntax {
  
  sealed trait UnaryOp
  case object Car extends UnaryOp
  case object Cdr extends UnaryOp
  
  sealed trait BinaryOp
  case object Cons extends BinaryOp
  
  sealed trait Relation
  case class Equals(left: Expression, right: Expression) extends Relation
  case class IsEmpty(expr: Expression) extends Relation
  
  sealed trait Expression
  case class Var(name: String) extends Expression
  case class If(cond: Relation, left: Expression, right: Expression) extends Expression
  case class Call(name: String, arguments: List[Expression]) extends Expression
  case class Unary(op: UnaryOp, expr: Expression) extends Expression
  case class Binary(op: BinaryOp, left: Expression, right: Expression) extends Expression
  
  case class Equation(name: String, parameters: List[String], body: Expression)
  case class Program(definitions: List[Equation])
  
}