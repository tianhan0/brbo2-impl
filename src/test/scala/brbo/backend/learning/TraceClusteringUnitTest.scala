package brbo.backend.learning

import brbo.TestCase
import brbo.backend.interpreter.Interpreter
import brbo.backend.interpreter.Interpreter.{ResetNode, UseNode}
import brbo.common.ast._
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class TraceClusteringUnitTest extends AnyFlatSpec {
  "Computing the distances between traces" should "be correct" in {
    TraceClusteringUnitTest.distanceMatrixTests.foreach({
      testCase =>
        val matrix = TraceClustering.distanceMatrix(traces = testCase.input.asInstanceOf[List[Interpreter.CostTrace]], substitutionPenalty = 100)
        StringCompare.ignoreWhitespaces(matrix.toString(), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Writing matrices into JSON" should "be correct" in {
    TraceClusteringUnitTest.jsonWriteTests.foreach({
      testCase =>
        val json = TraceClustering.matrixToJson(testCase.input.asInstanceOf[List[List[Int]]])
        StringCompare.ignoreWhitespaces(json, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object TraceClusteringUnitTest {
  val distanceMatrixTests: List[TestCase] = {
    val use1 = UseNode(Use(Some(1), Number(1)), 1)
    val use1p = UseNode(Use(Some(1), Number(1)), 1)
    val use2 = UseNode(Use(Some(1), Number(2)), 2)
    val use3 = UseNode(Use(Some(1), Number(3)), 3)
    val use4 = UseNode(Use(Some(1), Number(4)), 4)
    val reset = ResetNode(Reset(2))

    val trace1 = List(use1, use1p, use2)
    val trace2 = List(use1p, use2, use1, reset)
    val trace3 = List(use1, use2, use3)
    val trace4 = List(use1, use2, use3, use3, use4)
    val trace5 = List(use1, reset)
    val traceList1 = List(trace1, trace2).map(nodes => Interpreter.CostTrace(nodes))
    val traceList2 = List(trace1, trace3).map(nodes => Interpreter.CostTrace(nodes))
    val traceList3 = List(trace1, trace4).map(nodes => Interpreter.CostTrace(nodes))
    val traceList4 = List(trace1, trace5).map(nodes => Interpreter.CostTrace(nodes))
    val traceList5 = List(trace2, trace3).map(nodes => Interpreter.CostTrace(nodes))
    val traceList6 = List(trace2, trace4).map(nodes => Interpreter.CostTrace(nodes))
    val traceList7 = List(trace2, trace5).map(nodes => Interpreter.CostTrace(nodes))
    val traceList8 = List(trace3, trace4).map(nodes => Interpreter.CostTrace(nodes))
    val traceList9 = List(trace3, trace5).map(nodes => Interpreter.CostTrace(nodes))
    val traceList10 = List(trace4, trace5).map(nodes => Interpreter.CostTrace(nodes))
    val traceList11 = List(trace5, trace5).map(nodes => Interpreter.CostTrace(nodes))

    List(
      TestCase("traces1", traceList1, """List(List(0, 0), List(0, 0))"""),
      TestCase("traces2", traceList2, """List(List(0, 100), List(100, 0))"""),
      TestCase("traces3", traceList3, """List(List(0, 100), List(100, 0))"""),
      TestCase("traces4", traceList4, """List(List(0, 0), List(0, 0))"""),
      TestCase("traces5", traceList5, """List(List(0, 100), List(100, 0))"""),
      TestCase("traces6", traceList6, """List(List(0, 100), List(100, 0))"""),
      TestCase("traces7", traceList7, """List(List(0, 0), List(0, 0))"""),
      TestCase("traces8", traceList8, """List(List(0, 0), List(0, 0))"""),
      TestCase("traces9", traceList9, """List(List(0, 0), List(0, 0))"""),
      TestCase("traces10", traceList10, """List(List(0, 0), List(0, 0))"""),
      TestCase("traces11", traceList11, """List(List(0, 0), List(0, 0))"""),
    )
  }

  val jsonWriteTests: List[TestCase] = {
    val matrix1 = List(List(1, 2), List(3, 4, 5, 6))
    List(TestCase("matrix1", matrix1, """"""))
  }
}
