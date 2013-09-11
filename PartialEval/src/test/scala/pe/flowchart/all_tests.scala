package pe.flowchart

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Suites

@RunWith(classOf[JUnitRunner])
class FlowChartSuites extends Suites(
    new FlowChartParserSuite,
    new FlowChartInterpreterSuite,
    new FlowChartSpecializerSuite
)

