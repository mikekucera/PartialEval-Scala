package pe.flowchart

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

import scala.collection.immutable.SortedMap

import pe.flowchart.FlowChartInterpreter.{Value, StringValue, ListValue}
import pe.flowchart.FlowChartSpecializer._ 
import pe.flowchart.FlowChartParser._
import pe.flowchart.FlowChartSyntax.pretty

@RunWith(classOf[JUnitRunner])
class FlowChartSpecializerSuite extends FunSuite with MustMatchers {
  
  test("does it work at all?") {
    val path = "src/test/scala/pe/flowchart/lookup.flow"
    val source = io.Source.fromFile(path).mkString
    val ast = parseAll(program, source).get
    val input = SortedMap("name" -> StringValue("c"), "namelist" -> ListValue("a","b","c"))
    val reducedProgram = specialize(ast, input)
    
    println(pretty(reducedProgram))
  }
  
  
}