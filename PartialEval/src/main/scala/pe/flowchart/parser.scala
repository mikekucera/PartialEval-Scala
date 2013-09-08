package pe.flowchart

import scala.util.parsing.combinator._
import pe.flowchart.FlowChartSyntax._


object FlowChartParser extends RegexParsers {
  
  def string: Parser[String] = "'\\w+".r ^^ (_.substring(1))
  def ident:  Parser[String] = "[A-Za-z](\\w|-)*".r
  def label:  Parser[String] = ident
  
  // add support for C-style multi-line comments (from http://stackoverflow.com/questions/5952720/ignoring-c-style-comments-in-a-scala-combinator-parser)
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  
  def unaryOp: Parser[UnaryOp] = 
    "hd" ^^ (_ => Head) | 
    "tl" ^^ (_ => Tail) | 
    "first_sym" ^^ (_ => FirstSym)
  
  def binaryOp: Parser[BinaryOp] = 
    "cons" ^^ (_ => Cons) |
    "new_tail" ^^ (_ => NewTail)
                                   
  def expression: Parser[Expression] = 
    string ^^ Str |
    "'(" ~> repsep(string,",") <~ ")" ^^ Lst |
    unaryOp ~ ("(" ~> expression <~ ")") ^^ { case op~e => Unary(op, e) } |
    binaryOp ~ ("(" ~> expression) ~ ("," ~> expression <~ ")") ^^ { case op~e1~e2 => Binary(op, e1, e2) } |
    ident ^^ Var
                                   
  def branchCommand: Parser[Command] =
    "goto" ~> label <~ ";" ^^ Goto |
    "return" ~> expression <~ ";" ^^ Return |
    ("if" ~> expression) ~ ("=" ~> expression <~ "goto") ~ (label <~ "else") ~ label <~ ";" ^^ { case e1~e2~l1~l2 => IfGotoElse(e1,e2,l1,l2) }
    
  def assign: Parser[Command] =
    ident ~ (":=" ~> expression <~ ";") ^^ { case ident~expr => Assign(ident, expr) }
    
  def block: Parser[List[Line]] =
    label ~ (":" ~> rep(assign)) ~ branchCommand ^^ { 
      case label~assigns~branch => assigns :+ branch match {
        case c::cs => Line(label,c) :: cs.map(Line("",_))
      }
    }
    
  def read: Parser[Read] =
    "read" ~> "(" ~> repsep(ident,",") <~ ")" <~ ";" ^^ Read
    
  def program: Parser[Program] =
    read ~ rep(block) ^^ { case read~blocks => Program(read, blocks.flatten) }
  
}