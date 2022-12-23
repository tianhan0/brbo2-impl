package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.Driver
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.ScriptRunner.KMeans
import brbo.backend2.learning.SegmentClustering.Segment
import brbo.backend2.learning.SegmentClusteringUnitTest._
import brbo.common.BrboType.INT
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.scalatest.flatspec.AnyFlatSpec

class SegmentClusteringUnitTest extends AnyFlatSpec {
  "Clustering similar segments with KMeans" should "be correct" in {
    SegmentClusteringUnitTest.clusterSimilarSegmentTests.foreach({
      testCase =>
        val (program, inputs) = testCase.input.asInstanceOf[(String, List[BrboValue])]
        val interpreter = SegmentClusteringUnitTest.toInterpreter(program)
        val trace = getTrace(interpreter, inputs)
        val segmentClustering = new SegmentClustering(sumWeight = 1000, commandWeight = 10, debugMode = false, algorithm = KMeans(clusters = Some(5)), threads = SegmentClustering.THREADS)
        val candidateSegments: List[Segment] = segmentClustering.generateSegments(trace, segmentLength = 1, excludeIndices = Set())
        val groups = segmentClustering.clusterSimilarSegments(trace, candidateSegments)
        StringCompare.ignoreWhitespaces(printSegments(groups, trace), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Deciding the overlapping between segments" should "be correct" in {
    SegmentClusteringUnitTest.segmentOverlapTest.foreach({
      testCase =>
        val (s1, s2) = testCase.input.asInstanceOf[(Segment, Segment)]
        StringCompare.ignoreWhitespaces(s1.notOverlap(s2).toString, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Computing the similarity between values" should "be correct" in {
    SegmentClusteringUnitTest.valueSimilarityTest.foreach({
      testCase =>
        val (value, values) = testCase.input.asInstanceOf[(SegmentClustering.Value, List[SegmentClustering.Value])]
        val similarities =
          SegmentClustering.Value.similarity(value = value, values = values, maxSimilarity = 100, minSimilarity = 0).map(similarity => similarity.toInt)
        StringCompare.ignoreWhitespaces(similarities, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object SegmentClusteringUnitTest {
  def printSegments(groups: List[List[Segment]], trace: Trace): String = {
    groups.map(group => group.map(segment => segment.print(trace))).mkString("\n")
  }

  def getTrace(interpreter: Interpreter, inputs: List[BrboValue]): Trace = {
    val flowEndState = interpreter.execute(inputs)
    flowEndState.trace
  }

  def toInterpreter(source: String): Interpreter = {
    val targetProgram = BasicProcessor.getTargetProgram(className = "Test", sourceCode = source).program
    new Interpreter(Driver.insertResetPlaceHolders(targetProgram))
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
       |  void ${TargetProgram.MAIN_FUNCTION}(int n) {
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
       |  void ${TargetProgram.MAIN_FUNCTION}(int[] array) {
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
       |  void ${TargetProgram.MAIN_FUNCTION}(int[] array, int n) {
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
       |  void ${TargetProgram.MAIN_FUNCTION}(int[] array1, int[] array2, int n) {
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
        |List(use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011))
        |List(use R0 88 (cost=88), use R0 88 (cost=88))
        |List(use R0 89 (cost=89), use R0 89 (cost=89))""".stripMargin),
    TestCase("amortizeAndWorstCase01", (amortizeAndWorstCase01, List(BrboArray(List(Number(10), Number(4), Number(3)), INT))),
      """List(use R0 arrayRead(array, i) (cost=10))
        |List(use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011))
        |List(use R0 arrayRead(array, i) (cost=4))
        |List(use R0 arrayRead(array, i) (cost=3))
        |List(use R0 88 (cost=88))""".stripMargin),
    TestCase("amortizeAndWorstCase02", (amortizeAndWorstCase02, List(
      BrboArray(List(Number(10), Number(4), Number(3)), INT),
      Number(3)
    )),
      """List(use R0 arrayRead(array, j) (cost=3), use R0 arrayRead(array, j) (cost=3), use R0 arrayRead(array, j) (cost=3))
        |List(use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011), use R0 1011 (cost=1011))
        |List(use R0 88 (cost=88), use R0 88 (cost=88), use R0 88 (cost=88))
        |List(use R0 arrayRead(array, j) (cost=10), use R0 arrayRead(array, j) (cost=10), use R0 arrayRead(array, j) (cost=10))
        |List(use R0 arrayRead(array, j) (cost=4), use R0 arrayRead(array, j) (cost=4), use R0 arrayRead(array, j) (cost=4))""".stripMargin),
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

  val segmentOverlapTest: List[TestCase] = {
    val s1 = Segment(List(100, 145))
    val s2 = Segment(List(103, 142))
    val s3 = Segment(List(99, 100))
    val s4 = Segment(List(90, 140))
    List(
      TestCase("Test 01", (s1, s2), "false"),
      TestCase("Test 02", (s1, s3), "false"),
      TestCase("Test 03", (s1, s4), "false"),
      TestCase("Test 04", (s2, s3), "true"),
      TestCase("Test 05", (s2, s4), "false"),
      TestCase("Test 06", (s3, s4), "false"),
    )
  }

  val valueSimilarityTest: List[TestCase] = {
    val one = SegmentClustering.IntegerValue(1)
    val two = SegmentClustering.IntegerValue(2)
    val array1 = SegmentClustering.ArrayValue(List(4, 5, 6))
    val array2 = SegmentClustering.ArrayValue(List(5, 7, 8, 9))
    val array3 = SegmentClustering.ArrayValue(List(1, 2, 3, 4))
    List(
      TestCase("Test 01", (one, List(array1)), """0"""),
      TestCase("Test 02", (two, List(array3)), """25"""),
      TestCase("Test 03", (array1, List(array1)), """100"""),
      TestCase("Test 04", (array1, List(array2)), """16"""),
      TestCase("Test 05", (array1, List(array3)), """16"""),
    )
  }
}
