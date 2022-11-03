package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.Clustering.KMeans
import brbo.backend2.learning.SegmentClustering.Segment
import brbo.backend2.learning.SegmentClusteringUnitTest._
import brbo.common.BrboType.INT
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.frontend.BasicProcessor
import org.scalatest.flatspec.AnyFlatSpec

class SegmentClusteringUnitTest extends AnyFlatSpec {
  "Clustering similar segments with KMeans" should "be correct" in {
    SegmentClusteringUnitTest.clusterSimilarSegmentTests.foreach({
      testCase =>
        val (program, inputs) = testCase.input.asInstanceOf[(String, List[BrboValue])]
        val trace = getTrace(program, inputs)
        val segmentClustering = new SegmentClustering(sumWeight = 1000, commandWeight = 10, debugMode = false)
        val groups = segmentClustering.clusterSimilarSegments(trace, segmentLength = 1, KMeans(Some(5)))
        StringCompare.ignoreWhitespaces(printSegments(groups, trace), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object SegmentClusteringUnitTest {
  def printSegments(groups: List[List[Segment]], trace: Trace): String = {
    groups.map(group => group.map(segment => segment.print(trace.costTraceAssociation))).mkString("\n")
  }

  def toBrboProgram(program: String): BrboProgram = {
    val targetProgram = BasicProcessor.getTargetProgram("Test", program).program
    val mainFunctionWithResetPlaceHolders =
      targetProgram.mainFunction.replaceBodyWithoutInitialization(
        BrboAstUtils.insertResetPlaceHolder(targetProgram.mainFunction.body).asInstanceOf[Statement]
      )
    targetProgram.replaceMainFunction(mainFunctionWithResetPlaceHolders)
  }

  def getTrace(program: String, inputs: List[BrboValue]): Trace = {
    val interpreter = new Interpreter(toBrboProgram(program), debugMode = true)
    val flowEndState = interpreter.execute(inputs)
    flowEndState.trace
  }

  val functionDefinitions: String =
    """  abstract void use(int x, int cost, boolean condition);
      |  abstract void use(int x, int cost);
      |  abstract void reset(int x, boolean condition);
      |  abstract void reset(int x);
      |  abstract int arrayRead(int[] x, int index);
      |  abstract int arrayLength(int[] x);""".stripMargin
  private val loopPhase =
    s"""abstract class Test {
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
    s"""abstract class Test {
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
    s"""abstract class Test {
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
    s"""abstract class Test {
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

  val clusterSimilarSegmentTests: List[TestCase] = List(
    TestCase("loopPhase", (loopPhase, List(Number(4))),
      """List(use R0 2011 (cost=2011))
        |List(use R0 1011 (cost=1011))
        |List(use R0 88 (cost=88), use R0 88 (cost=88))
        |List(use R0 89 (cost=89), use R0 89 (cost=89))""".stripMargin),
    TestCase("amortizeAndWorstCase01", (amortizeAndWorstCase01, List(BrboArray(List(Number(10), Number(4), Number(3)), INT))),
      """List(use R0 arrayRead(array, i) (cost=10))
        |List(use R0 arrayRead(array, i) (cost=4))
        |List(use R0 arrayRead(array, i) (cost=3))
        |List(use R0 1011 (cost=1011))
        |List(use R0 88 (cost=88))""".stripMargin),
    TestCase("amortizeAndWorstCase02", (amortizeAndWorstCase02, List(
      BrboArray(List(Number(10), Number(4), Number(3)), INT),
      Number(3)
    )),
      """List(use R0 arrayRead(array, j) (cost=10))
        |List(use R0 arrayRead(array, j) (cost=4))
        |List(use R0 arrayRead(array, j) (cost=3))
        |List(use R0 1011 (cost=1011))
        |List(use R0 88 (cost=88))""".stripMargin),
    TestCase("amortizeSeparately", (amortizeSeparately, List(
      BrboArray(List(Number(10), Number(4), Number(3)), INT),
      BrboArray(List(Number(7), Number(20), Number(1)), INT),
      Number(3)
    )),
      """List(use R0 arrayRead(array1, i) (cost=10))
        |List(use R0 arrayRead(array2, i) (cost=7))
        |List(use R0 arrayRead(array1, i) (cost=4), use R0 arrayRead(array1, i) (cost=3))
        |List(use R0 arrayRead(array2, i) (cost=20))
        |List(use R0 arrayRead(array2, i) (cost=1))""".stripMargin),
  )
}
