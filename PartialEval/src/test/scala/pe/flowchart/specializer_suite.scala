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
  
  test("does it work at all?") {
    println(pretty(lookup))
    println()
    
    // specialize program
    val static = Map("name" -> StringValue("c"), "namelist" -> ListValue("a","b","c"))
    val specializedProgram = specialize(lookup, static)
    
    println(pretty(specializedProgram))
    
    
    val remainingInput = List(ListValue("1","2","3","4"))
    val result = runProgram(specializedProgram, remainingInput)
    
    result must be (StringValue("3"))
  }
  
  
}