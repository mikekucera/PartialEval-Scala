package pe.schemezero

import pe.schemezero.SchemeZeroSyntax._
import scala.annotation.tailrec

/**
 * Scheme is relatively easy to parse therefore the Scheme0 parser is
 * hand written as parser combinators are not really required.
 * 
 * The parse has three phases:
 * 1) Tokenize the input
 * 2) Parse the token list into a generic s-expression tree. This phase
 *    ensures that all brackets are balanced.
 * 3) Parse the s-expression tree into a more structured Scheme0 AST.
 */
object SchemeZeroParser {
  
  
  private val wrap =  "(" + (_:String) + ")"
  
  val parse = wrap andThen tokenize andThen parseSExpr andThen parseProgram
 
  
  /**
   * S-expression trees.
   */
  sealed trait SExpr
  case class Atom(value: String) extends SExpr
  case class Lst(values: List[SExpr]) extends SExpr
  object Lst {
    def apply(args: SExpr*): SExpr = Lst(args.toList)
  }
  
  /**
   * I find it simpler to fail the parse with an exception rather than use Try or Either everywhere.
   */
  class FailParseException(message: String) extends RuntimeException(message)
  
  
  /**
   * Tokenize by just splitting the input up by whitespace.
   */
  def tokenize(source: String): List[String] =
    source.replace("(", " ( ").replace(")", " ) ").split("\\s+").toList.drop(1)
      
    
  /**
   * Parse the source in to a simple tree of s-expressions.
   */
  def parseSExpr(tokens: List[String]): SExpr = {
    @tailrec
    def parseTokens(tokens: List[String], stack: List[List[SExpr]]): SExpr = (tokens, stack) match {
	  case ("(" :: ts,  stack)            => parseTokens(ts, Nil::stack)
	  case (")" :: Nil, List(program))    => Lst(program.reverse)
	  case (")" :: ts,  head::next::rest) => parseTokens(ts, (Lst(head.reverse)::next)::rest)
	  case (atm :: ts,  head::tail)       => parseTokens(ts, (Atom(atm)::head)::tail)
	  case _ => throw new FailParseException(tokens.take(10).mkString(" "))
	}
    parseTokens(tokens, Nil) 
  }
  
  
  /**
   * "Parse" the s-expression tree to find the program structure.
   */
  def parseProgram(sexpr: SExpr): Program = sexpr match {
    case Lst(equations) => Program(program(equations))
    case _ => throw new FailParseException("invalid program")
  }
  
  def program(equations: List[SExpr]): List[Equation] = equations match {
    case eq::rest => equation(eq) :: program(rest)
    case Nil => Nil
  }
  
  def equation(sexpr: SExpr): Equation = sexpr match {
    case Lst(Atom("define")::args::body::Nil) => Equation(name(args), parameters(args), expression(body))
    case x => throw new FailParseException("expecting equation, got " + x)
  }
  
  def name(sexpr: SExpr): String = sexpr match {
    case Lst(Atom(name)::parameters) => name
    case _ => throw new FailParseException("expecting equation name")
  }
  
  def parameters(sexpr: SExpr): List[String] = sexpr match {
    case Lst(Atom(name)::parameters) => parameters collect {
      case Atom(param) => param
      case Lst(_) => throw new FailParseException("parameter expected")
    }
    case _ => throw new FailParseException("expecting parameters")
  }
  
  def expression(sexpr: SExpr): Expression = sexpr match {
    case Atom(name) => Var(name)
    case Lst(Atom("if")::cond::left::right::Nil) => If(relation(cond), expression(left), expression(right))
    case Lst(Atom("call")::Atom(name)::args) => Call(name, args.map(expression))
    case Lst(Atom("car")::expr::Nil) => Car(expression(expr))
    case Lst(Atom("cdr")::expr::Nil) => Cdr(expression(expr))
    case Lst(Atom("cons")::left::right::Nil) => Cons(expression(left), expression(right))
    case _ => throw new FailParseException("expecting expression")
  }
  
  def relation(sexpr: SExpr): Relation = sexpr match {
    case Lst(Atom("equals")::left::right::Nil) => Equals(expression(left), expression(right))
    case Lst(Atom("isEmpty")::expr::Nil) => IsEmpty(expression(expr))
    case _ => throw new FailParseException("expecting relation")
  }
  
}