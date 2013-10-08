package pe.flowchart

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers._
import org.scalatest.exceptions.TestFailedException

import pe.flowchart.FlowChartSyntax._
import pe.flowchart.FlowChartParser._ 

@RunWith(classOf[JUnitRunner])
class FlowChartParserSuite extends FunSuite with MustMatchers {
  
  val failParsing = not be ('successful)
  
  implicit class ParseAs(source:String) {
    def tryParsingAs[T](parser: Parser[T]): ParseResult[T] = parseAll(parser, source)
    def parseAs[T](parser: Parser[T]): T = tryParsingAs(parser) match {
      case Success(result, _) => result
      case Error(message, _) => fail(message)
      case Failure(message, _) => fail(message)
    }
  }

  
  test("parse unary keywords") {
    "hd" parseAs unaryOp must equal (Head)
    "tl" parseAs unaryOp must equal (Tail)
    "first_sym" parseAs unaryOp must equal (FirstSym)
    "blah" tryParsingAs unaryOp must failParsing
  }
  
  test("parse binary keywords") {
    "cons" parseAs binaryOp must equal (Cons)
    "new_tail" parseAs binaryOp must equal (NewTail)
    "blah" tryParsingAs binaryOp must failParsing
  }
  
  test("parse strings") {
    "'asdf'" parseAs string must equal ("asdf")
    "'12'" parseAs string must equal ("12")
    "'a1_a1_'" parseAs string must equal ("a1_a1_")
    "asdf" tryParsingAs string must failParsing
    "'a$" tryParsingAs string must failParsing
    "' '" parseAs string must equal (" ")
    "''" parseAs string must equal ("")
  }
  
  test("parse identifiers") {
    "a1_" parseAs ident must equal ("a1_")
    "do-right" parseAs ident must equal ("do-right")
    "1a" tryParsingAs ident must failParsing
  }
  
  test("parse simple expressions") {
    "'as_1'" parseAs expression must equal (Str("as_1"))
    "[]" parseAs expression must equal (Lst(List()))
    "['a','b']" parseAs expression must equal(Lst(List("a","b")))
    "as_1" parseAs expression must equal (Var("as_1"))
    "1a" tryParsingAs expression must failParsing
  }

  test("parse unary expressions") {
    "first_sym ( xs ) " parseAs expression must equal (Unary(FirstSym,Var("xs")))
    "hd(tl(tl(inst)))" parseAs expression must equal (Unary(Head,Unary(Tail,Unary(Tail,Var("inst")))))
    "hd tl inst" tryParsingAs expression must failParsing
  }
  
  test("parse binary expressions") {
    "cons(x, [])" parseAs expression must equal (Binary(Cons,Var("x"),Lst(List())))
    "cons(x, cons(y, []))" parseAs expression must equal (Binary(Cons,Var("x"),Binary(Cons,Var("y"),Lst(List()))))
    "cons(x,y,z)" tryParsingAs expression must failParsing
    "new_tail(x,y)" parseAs expression must equal (Binary(NewTail,Var("x"),Var("y")))
    "split(x,y)" parseAs expression must equal (Binary(Split,Var("x"),Var("y")))
  }
  
  test("parse assignment") {
    "x := y;" parseAs assign must equal (Assign("x",Var("y")))
    "abc := hd(xs);" parseAs assign must equal (Assign("abc",Unary(Head,Var("xs"))))
  }
  
  test("parse goto") {
    "goto do-it;" parseAs branchCommand must equal (Goto("do-it"))
    "goto what" tryParsingAs branchCommand must failParsing // missing semicolon
  }
  
  test("parse return") {
    "return right;" parseAs branchCommand must equal (Return(Var("right")))
  }
  
  test("parse if-goto-else") {
    "if x = y goto do-x else do-y;" parseAs branchCommand must equal (IfGotoElse(Var("x"),Var("y"),"do-x","do-y"))
    "loop: if Qtail = [] goto stop else cont;" tryParsingAs block must be ('successful)
  }
  
  test("parse blocks") {
    "loop: x := y; goto loop;" parseAs block must equal (List(Line("loop",Assign("x",Var("y"))),Line("",Goto("loop"))))
    "x := y; goto wat;" tryParsingAs block must failParsing // block must start with a label
    "loop: x := y; thegoto: goto loop;" tryParsingAs block must failParsing // block must contain only one label
    "loop: x :=y; z := q;" tryParsingAs block must failParsing // block must end with a branch
  }
  
  test("parse read") {
    "read (namelist, valuelist);" parseAs read must equal (Read(List("namelist","valuelist")))
  }
  
  test("parse stuff with comments") {
    "hd(/*the head of xs */ xs)" parseAs expression must equal (Unary(Head,Var("xs")))
    "/* set y to x */ x := y; /* yes */" parseAs assign must equal (Assign("x", Var("y")))
  }
  
  test("program fragment from section 4.4.2: lookup.flow") {
    val path = "src/test/scala/pe/flowchart/lookup.flow"
    val source = io.Source.fromFile(path).mkString
    source tryParsingAs program must be ('successful)
  }
  
  test("turing machine simulator program: turing.flow") {
    val path = "src/test/scala/pe/flowchart/turing.flow"
    val source = io.Source.fromFile(path).mkString
    source tryParsingAs program must be ('successful)
  }
  
}