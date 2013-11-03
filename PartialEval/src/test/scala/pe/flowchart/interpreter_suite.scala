package pe.flowchart

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import pe.flowchart.FlowChartInterpreter._ 
import pe.flowchart.FlowChartParser.{parseAll,program}

@RunWith(classOf[JUnitRunner])
class FlowChartInterpreterSuite extends FunSuite with MustMatchers {
  
  def getAst(filePath:String) = parseAll(program, new java.io.FileReader(filePath)).get
  
  val lookup = getAst("src/test/scala/pe/flowchart/lookup.flow")
  val turing = getAst("src/test/scala/pe/flowchart/turing.flow")
  
	  
  test("correct execution") {
    val input = List(StringValue("c"), ListValue("a","b","c","d"), ListValue("1","2","3","4"))
    val result = runProgram(lookup, input)
    result must be (StringValue("3"))
  }
  
  test("not enough input") {
    val input = List(StringValue("c"))
    val thrown = evaluating { runProgram(lookup, input) } must produce [EvaluationException]
    thrown.getMessage must equal ("not enough input parameters")
  }
  
  test("wrong type") {
    val input = List(StringValue("c"), StringValue("a"), ListValue("1","2","3","4"))
    val thrown = evaluating { runProgram(lookup, input) } must produce [EvaluationException]
    thrown.getMessage must equal ("expected a list")
  }
  
  test("run the turing machine") {
    val Q = ListValue("0 if 0 goto 3", "1 right", "2 goto 0", "3 write 1")
    val right = ListValue("1","1","0","1","0","1")
    val input = List(Q, right)
    val result = runProgram(turing, input)
    result must be (ListValue("1","1","0","1"))
  }
  
 
}