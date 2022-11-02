package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.SegmentClustering.Segment
import brbo.backend2.learning.SegmentClusteringUnitTest._
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.frontend.BasicProcessor
import org.scalatest.flatspec.AnyFlatSpec

class SegmentClusteringUnitTest extends AnyFlatSpec {
  private val loopPhaseProgram = BasicProcessor.getTargetProgram("Test", loopPhase).program
  private val amortizeAndWorstCase01Program = BasicProcessor.getTargetProgram("Test", amortizeAndWorstCase01).program
  private val amortizeAndWorstCase02Program = BasicProcessor.getTargetProgram("Test", amortizeAndWorstCase02).program
  private val amortizeSeparatelyProgram = BasicProcessor.getTargetProgram("Test", amortizeSeparately).program

  def printSegments(groups: List[List[Segment]], trace: Trace): String = {
    groups.map(group => group.map(segment => segment.print(trace.costTrace))).mkString("\n")
  }

  "Clustering similar segments" should "be correct" in {
    val interpreter = new Interpreter(loopPhaseProgram, debugMode = false)
    val flowEndState = interpreter.execute(List(Number(4)))
    val segmentClustering = new SegmentClustering(sumWeight = 1000, commandWeight = 10, debugMode = true)
    val groups = segmentClustering.clusterSimilarSegments(flowEndState.trace, segmentLength = 1)
    val expectedOutput =
      """List(use R0 1011)
        |List(use R0 2011)
        |List(use R0 88, use R0 88)
        |List(use R0 89, use R0 89)""".stripMargin
    StringCompare.ignoreWhitespaces(printSegments(groups, flowEndState.trace), expectedOutput, "loopPhaseProgram failed")
    // TODO: Test on more programs
  }
}

object SegmentClusteringUnitTest {
  private val functionDefinitions =
    """  void use(int x, int cost, boolean condition) {}
      |  void use(int x, int cost) {}
      |  void reset(int x, boolean condition) {}
      |  void reset(int x) {}
      |  int arrayRead(int[] x, int index) { return 0; }
      |  int arrayLength(int[] x) { return 0; }""".stripMargin
  private val loopPhase =
    s"""class Test {
       |  void main(int n) {
       |    int i = 0;
       |    while (i < n) {
       |      if (i != 0)
       |        use(0, 1011);
       |      else
       |        use(0, 2011);
       |      i++;
       |    }
       |    use(0, 88);
       |    use(0, 89);
       |    use(0, 88);
       |    use(0, 89);
       |  }
       |$functionDefinitions
       |}""".stripMargin
  private val amortizeAndWorstCase01 =
    s"""class Test {
       |  void main(int[] array) {
       |    int i = 0;
       |    while (i < arrayLength(array)) {
       |      use(0, arrayRead(array, i));
       |      use(0, 1011);
       |      i++;
       |    }
       |    use(0, 88);
       |  }
       |$functionDefinitions
       |}""".stripMargin
  private val amortizeAndWorstCase02 =
    s"""class Test {
       |  void main(int[] array, int n) {
       |    int i = 0;
       |    while (i < n) {
       |      int j = 0;
       |      while (j < arrayLength(array)) {
       |        use(0, arrayRead(array, j));
       |        use(0, 1011);
       |        j++;
       |      }
       |      use(0, 88);
       |      i++;
       |    }
       |  }
       |$functionDefinitions
       |}""".stripMargin
  private val amortizeSeparately =
    s"""class Test {
       |  void main(int[] array1, int[] array2, int n) {
       |    int i = 0;
       |    while (i < n) {
       |      use(0, arrayRead(array1, i));
       |      use(0, arrayRead(array2, i));
       |      i++;
       |    }
       |  }
       |$functionDefinitions
       |}""".stripMargin


  private val list1 = List(1, 1, 2, 2)

  val decomposeTests: List[TestCase] = List(
    TestCase("list1", (list1, 10), """""")
  )
}
