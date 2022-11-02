package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter._
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

  "Grouping same traces" should "be correct" in {
    TraceClusteringUnitTest.groupSameTraceTests.foreach({
      testCase =>
        val clusters = TraceClustering.groupZeroDistanceTraces(traces = testCase.input.asInstanceOf[List[Interpreter.CostTrace]])
        val string = clusters.values.map(set => set.map(t => t.print()).toList.sorted)
        StringCompare.ignoreWhitespaces(string, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Selecting representative traces" should "be correct" in {
    TraceClusteringUnitTest.selectRepresentativeTraceTests.foreach({
      testCase =>
        val representative = TraceClustering.selectRepresentativeCostTrace(testCase.input.asInstanceOf[List[Interpreter.CostTrace]])
        StringCompare.ignoreWhitespaces(representative.print(), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object TraceClusteringUnitTest {
  private val use1 = UseNode(Use(Some(1), Number(1)), 1)
  private val use1p = UseNode(Use(Some(1), Number(1)), 1)
  private val use2 = UseNode(Use(Some(1), Number(2)), 2)
  private val use3 = UseNode(Use(Some(1), Number(3)), 3)
  private val use4 = UseNode(Use(Some(1), Number(4)), 4)

  private val trace1 = List(use1, use1p, use2)
  private val trace2 = List(use1p, use2, use1)
  private val trace3 = List(use1, use2, use3)
  private val trace4 = List(use1, use2, use3, use3, use4)
  private val trace5 = List(use1)
  private val traceList1 = List(trace1, trace2).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList2 = List(trace1, trace3).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList3 = List(trace1, trace4).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList4 = List(trace1, trace5).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList5 = List(trace2, trace3).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList6 = List(trace2, trace4).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList7 = List(trace2, trace5).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList8 = List(trace3, trace4).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList9 = List(trace3, trace5).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList10 = List(trace4, trace5).map(nodes => Interpreter.CostTrace(nodes))
  private val traceList11 = List(trace5, trace5).map(nodes => Interpreter.CostTrace(nodes))

  val distanceMatrixTests: List[TestCase] = List(
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

  val groupSameTraceTests: List[TestCase] = List(
    TestCase("traces1", traceList1,
      """List(Use Trace: use R1 1 (cost=1)
        |           use R1 1 (cost=1)
        |           use R1 2 (cost=2), Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 1 (cost=1))""".stripMargin),
    TestCase("traces2", traceList2,
      """List(Use Trace: use R1 1 (cost=1)
        |           use R1 1 (cost=1)
        |           use R1 2 (cost=2))
        |List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3))""".stripMargin),
    TestCase("traces3", traceList3,
      """List(Use Trace: use R1 1 (cost=1)
        |           use R1 1 (cost=1)
        |           use R1 2 (cost=2))
        |List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3)
        |           use R1 3 (cost=3)
        |           use R1 4 (cost=4))""".stripMargin),
    TestCase("traces4", traceList4,
      """List(Use Trace: use R1 1 (cost=1), Use Trace: use R1 1 (cost=1)
        |           use R1 1 (cost=1)
        |           use R1 2 (cost=2))""".stripMargin),
    TestCase("traces5", traceList5,
      """List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 1 (cost=1))
        |List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3))""".stripMargin),
    TestCase("traces6", traceList6,
      """List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 1 (cost=1))
        |List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3)
        |           use R1 3 (cost=3)
        |           use R1 4 (cost=4))""".stripMargin),
    TestCase("traces7", traceList7,
      """List(Use Trace: use R1 1 (cost=1), Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 1 (cost=1))""".stripMargin),
    TestCase("traces8", traceList8,
      """List(Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3), Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3)
        |           use R1 3 (cost=3)
        |           use R1 4 (cost=4))""".stripMargin),
    TestCase("traces9", traceList9,
      """List(Use Trace: use R1 1 (cost=1), Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3))""".stripMargin),
    TestCase("traces10", traceList10,
      """List(Use Trace: use R1 1 (cost=1), Use Trace: use R1 1 (cost=1)
        |           use R1 2 (cost=2)
        |           use R1 3 (cost=3)
        |           use R1 3 (cost=3)
        |           use R1 4 (cost=4))""".stripMargin),
    TestCase("traces11", traceList11,
      """List(Use Trace: use R1 1 (cost=1))""".stripMargin),
  )

  val selectRepresentativeTraceTests: List[TestCase] = {
    List(
      TestCase("traceList1", traceList1,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 1 (cost=1)""".stripMargin),
      TestCase("traceList2", traceList2,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)""".stripMargin),
      TestCase("traceList3", traceList3,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)
          |           use R1 3 (cost=3)
          |           use R1 4 (cost=4)""".stripMargin),
      TestCase("traceList4", traceList4,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 1 (cost=1)
          |           use R1 2 (cost=2)""".stripMargin),
      TestCase("traceList5", traceList5,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)""".stripMargin),
      TestCase("traceList6", traceList6,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)
          |           use R1 3 (cost=3)
          |           use R1 4 (cost=4)""".stripMargin),
      TestCase("traceList7", traceList7,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 1 (cost=1)""".stripMargin),
      TestCase("traceList8", traceList8,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)
          |           use R1 3 (cost=3)
          |           use R1 4 (cost=4)""".stripMargin),
      TestCase("traceList9", traceList9,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)""".stripMargin),
      TestCase("traceList10", traceList10,
        """Use Trace: use R1 1 (cost=1)
          |           use R1 2 (cost=2)
          |           use R1 3 (cost=3)
          |           use R1 3 (cost=3)
          |           use R1 4 (cost=4)""".stripMargin),
      TestCase("traceList11", traceList11,
        """Use Trace: use R1 1 (cost=1)""".stripMargin),
    )
  }
}
