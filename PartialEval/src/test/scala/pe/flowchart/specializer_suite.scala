package pe.flowchart

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import scala.collection.immutable.SortedMap

import pe.flowchart.FlowChartInterpreter._
import pe.flowchart.FlowChartSpecializer._ 
import pe.flowchart.FlowChartParser._
import pe.flowchart.FlowChartSyntax.pretty


@RunWith(classOf[JUnitRunner])
class FlowChartSpecializerSuite extends FunSuite with MustMatchers {
  
  def getAst(filePath:String) = parseAll(program, new java.io.FileReader(filePath)).get
  
  val lookup = getAst("src/test/scala/pe/flowchart/lookup.flow")
  val turing = getAst("src/test/scala/pe/flowchart/turing.flow")
  
  test("specialize the lookup program") {
    println()
    println(pretty(lookup))
    println()
    
    // specialize program
    val static = Map("name" -> StringValue("c"), "namelist" -> ListValue("a","b","c"))
    val specializedProgram = specialize(lookup, static)
    
    println(pretty(specializedProgram))
    
    // running the specialized program on the remaining input should product the correct output
    val remainingInput = List(ListValue("1","2","3","4"))
    val result = runProgram(specializedProgram, remainingInput)
    
    result must be (StringValue("3"))
  }
  
  
  test("first futamura projection: specialize the turing machine interpreter") {
    println()
    println(pretty(turing))
    println()
    
    val turingProgram = ListValue("0 if 0 goto 3", "1 right", "2 goto 0", "3 write 1");
    val static = Map("Q" -> turingProgram)
    val specializedProgram = specialize(turing, static)
    
    println(pretty(specializedProgram))
    
    val remainingInput = List(ListValue("1","1","0","1","0","1"))
    val result = runProgram(specializedProgram, remainingInput)
    
    result must be (ListValue("1","1","0","1"))
  }
  
}