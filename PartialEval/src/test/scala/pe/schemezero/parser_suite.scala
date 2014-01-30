package pe.schemezero

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers._

import pe.schemezero.SchemeZeroParser._

@RunWith(classOf[JUnitRunner])
class SchemeZeroParserSuite extends FunSuite with MustMatchers {
  
  test("tokenizer") {
    val source = "(asdf 45 !+ ))"
    tokenize(source) must be (List("(", "asdf", "45", "!+", ")", ")"))
    
    tokenize("") must be (Nil)
    tokenize(" ") must be (Nil)
  }
  
  test("parse valid s expressions") {
    val source1 = "((a b c) (d))"
    parseSExpr(tokenize(source1)) must be (Lst(Lst(Atom("a"),Atom("b"),Atom("c")),Lst(Atom("d"))))    
    val source2 = "((a b c) d)"
    parseSExpr(tokenize(source2)) must be (Lst(Lst(Atom("a"),Atom("b"),Atom("c")),Atom("d")))    
  }
  
  test("parse invalid s expressions") {
    intercept[FailParseException] {
      parseSExpr(tokenize("((abc)"))
    }
    intercept[FailParseException] {
      parseSExpr(tokenize(")(a)"))
    }
  }
  
  test("parse append from section 5.3") {
    val path = "src/test/scala/pe/schemezero/append.sz"
    val source = io.Source.fromFile(path).mkString
    val program = parse(source)
  }
}