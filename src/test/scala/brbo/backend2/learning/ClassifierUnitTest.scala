package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.Classifier._
import brbo.backend2.learning.ClassifierUnitTest.loopPhase
import brbo.backend2.learning.ScriptRunner.{Euclidean, Optics}
import brbo.backend2.learning.SegmentClustering.{Group, Segment}
import brbo.backend2.learning.SegmentClusteringUnitTest.functionDefinitions
import brbo.common.BrboType.INT
import brbo.common.ast.{Command, Identifier, Number}
import brbo.common.cfg.ControlFlowGraph
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class ClassifierUnitTest extends AnyFlatSpec {
  "Generate tables for classification" should "be correct" in {
    val interpreter = SegmentClusteringUnitTest.toInterpreter(loopPhase)
    val trace = SegmentClusteringUnitTest.getTrace(interpreter, List(Number(4)))
    // println(trace.toTable(printStores = false, onlyGhostCommand = false, omitExpressions = false, commandMaxLength = 30)._1.printAll())
    val groups1 = ClassifierUnitTest.generateGroups(trace, numberOfGroups = 2)
    val table1 = Classifier.generateTables(
      trace,
      Classifier.evaluateFromInterpreter(interpreter),
      groups1,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      throwIfNoResetPlaceHolder = true,
      controlFlowGraph = ControlFlowGraph.toControlFlowGraph(interpreter.brboProgram)
    )
    val decompositionExpected1 =
      """Index  |        Commands         |  Costs  |  SegmentIDs in GroupID(0)  |  SegmentIDs in GroupID(1)  |
        |-------------------------------------------------------------------------------------------------------
        |    12  |  resetPlaceHolder_1();  |         |                            |                            |
        |    19  |        R0 = R0 + 2011;  |   2011  |                            |                         0  |
        |    27  |  resetPlaceHolder_1();  |         |                            |                            |
        |    34  |        R0 = R0 + 1011;  |   1011  |                         0  |                            |
        |    42  |  resetPlaceHolder_1();  |         |                            |                            |
        |    49  |        R0 = R0 + 1011;  |   1011  |                            |                         1  |
        |    57  |  resetPlaceHolder_1();  |         |                            |                            |
        |    64  |        R0 = R0 + 1011;  |   1011  |                         1  |                            |""".stripMargin
    StringCompare.ignoreWhitespaces(SegmentClustering.printDecomposition(trace, groups1), decompositionExpected1, "decomposition 1 failed")

    val expected1 =
      """Tables:
        |Features: i, n
        |TraceLocation: resetPlaceHolder_1(); (index=12) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        |************************************************************
        |
        |TraceLocation: resetPlaceHolder_1(); (index=27) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 1  |  4  |  false  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 1  |  4  |   true  |
        |************************************************************
        |
        |TraceLocation: resetPlaceHolder_1(); (index=42) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 2  |  4  |   true  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 2  |  4  |  false  |
        |************************************************************
        |
        |TraceLocation: resetPlaceHolder_1(); (index=57) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 3  |  4  |  false  |
        |************************************************************
        |
        |TraceLocation: use R0 1011 (index=34) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 1  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |TraceLocation: use R0 1011 (index=49) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 2  |  4  |  GroupID(1)  |
        |************************************************************
        |
        |TraceLocation: use R0 1011 (index=64) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 3  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |TraceLocation: use R0 2011 (index=19) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 0  |  4  |  GroupID(1)  |
        |""".stripMargin
    StringCompare.ignoreWhitespaces(table1.print(), expected1, "loopPhase failed: Multiple groups")

    val groups2 = Map(groups1.head)
    val table2 = Classifier.generateTables(
      trace,
      Classifier.evaluateFromInterpreter(interpreter),
      groups2,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      throwIfNoResetPlaceHolder = true,
      controlFlowGraph = ControlFlowGraph.toControlFlowGraph(interpreter.brboProgram)
    )
    val decompositionExpected2 =
      """Index  |        Commands         |  Costs  |  SegmentIDs in GroupID(0)  |
        |--------------------------------------------------------------------------
        |    12  |  resetPlaceHolder_1();  |         |                            |
        |    19  |        R0 = R0 + 2011;  |   2011  |                            |
        |    27  |  resetPlaceHolder_1();  |         |                            |
        |    34  |        R0 = R0 + 1011;  |   1011  |                         0  |
        |    42  |  resetPlaceHolder_1();  |         |                            |
        |    49  |        R0 = R0 + 1011;  |   1011  |                            |
        |    57  |  resetPlaceHolder_1();  |         |                            |
        |    64  |        R0 = R0 + 1011;  |   1011  |                         1  |""".stripMargin
    StringCompare.ignoreWhitespaces(SegmentClustering.printDecomposition(trace, groups2), decompositionExpected2, "decomposition 2 failed")
    val expected2 =
      """Tables:
        |Features: i, n
        |TraceLocation: resetPlaceHolder_1(); (index=12) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        |************************************************************
        |
        |TraceLocation: resetPlaceHolder_1(); (index=27) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 1  |  4  |  false  |
        |************************************************************
        |
        |TraceLocation: resetPlaceHolder_1(); (index=42) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 2  |  4  |   true  |
        |************************************************************
        |
        |TraceLocation: resetPlaceHolder_1(); (index=57) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 3  |  4  |  false  |
        |************************************************************
        |
        |TraceLocation: use R0 1011 (index=34) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 1  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |TraceLocation: use R0 1011 (index=49) ->
        |Use table:
        | i  |  n  |    Label    |
        |-------------------------
        | 2  |  4  |  NoneGroup  |
        |************************************************************
        |
        |TraceLocation: use R0 1011 (index=64) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 3  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |TraceLocation: use R0 2011 (index=19) ->
        |Use table:
        | i  |  n  |    Label    |
        |-------------------------
        | 0  |  4  |  NoneGroup  |
        |""".stripMargin
    StringCompare.ignoreWhitespaces(table2.print(), expected2, "loopPhase failed: Single group")

    // TODO: More tests
  }

  "Learning classifiers from tables" should "be correct" in {
    ClassifierUnitTest.classifierTest.foreach({
      testCase =>
        val debugMode = false
        val (result: Classifier.ClassifierResult, featureNames) = {
          val table = testCase.input.asInstanceOf[BrboTable]
          (Classifier.classify(table, debugMode), table.featureNames)
        }
        StringCompare.ignoreWhitespaces(result.classifier.print(featureNames), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Deciding if traces satisfy the bounds under the given decompositions" should "be correct" in {
    val interpreter = SegmentClusteringUnitTest.toInterpreter(loopPhase)
    val trace = SegmentClusteringUnitTest.getTrace(interpreter, List(Number(4)))
    val groups = ClassifierUnitTest.generateGroups(trace, numberOfGroups = 2)
    val tables = Classifier.generateTables(
      trace,
      Classifier.evaluateFromInterpreter(interpreter),
      groups,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      throwIfNoResetPlaceHolder = false,
      controlFlowGraph = ControlFlowGraph.toControlFlowGraph(interpreter.brboProgram)
    )
    val classifierResults = tables.toProgramTables.generateClassifiers(debugMode = false)
    val invalidBound = Number(2000)
    val resultFalse = Classifier.applyClassifiers(
      Some(invalidBound),
      trace,
      evaluateFromInterpreter(interpreter),
      classifierResults,
      debugMode = false
    ).asInstanceOf[BoundCheckClassifierApplication].exceedBound
    StringCompare.ignoreWhitespaces(resultFalse.toString, "true", "Expected to not satisfy the bound")
    // println(SegmentClustering.printDecomposition(trace, groups))

    val validBound = Number(8000)
    val resultTrue = Classifier.applyClassifiers(
      Some(validBound),
      trace,
      evaluateFromInterpreter(interpreter),
      classifierResults,
      debugMode = false
    ).asInstanceOf[BoundCheckClassifierApplication].exceedBound
    StringCompare.ignoreWhitespaces(resultTrue.toString, "false", "Expected to satisfy the bound")
  }

  "Deciding if applying classifiers to a trace leads to segments with similar costs" should "be correct" in {
    val interpreter = SegmentClusteringUnitTest.toInterpreter(loopPhase)
    val trace = SegmentClusteringUnitTest.getTrace(interpreter, List(Number(10)))
    val algorithm = Optics(maxEps = Some(0.8), metric = Euclidean)
    val segmentClustering = new SegmentClustering(sumWeight = 1, commandWeight = 0, debugMode = false, algorithm, SegmentClustering.THREADS)
    val clusters: List[List[Segment]] = segmentClustering.clusterSimilarSegments(trace, 1, excludeIndices = Set())
    val group = Group(clusters.head.sortWith({ case (s1, s2) => s1.lessThan(s2) }))
    val result = segmentClustering.chooseGeneralizableGroups(List(group), testTrace = trace, similarTraces = List(trace), interpreter, sampleKTraces = None)
    val resultString = result.map(g => g.print(trace)).mkString("\n")
    StringCompare.ignoreWhitespaces(
      resultString,
      """use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011); use R0 1011 (cost=1011)""",
      "failed"
    )
  }

  "Generating program transformations from classifiers" should "be correct" in {
    val interpreter = SegmentClusteringUnitTest.toInterpreter(loopPhase)
    val trace = SegmentClusteringUnitTest.getTrace(interpreter, List(Number(4)))
    val groups = ClassifierUnitTest.generateGroups(trace, numberOfGroups = 2)
    val tables = Classifier.generateTables(
      trace,
      Classifier.evaluateFromInterpreter(interpreter),
      groups,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      throwIfNoResetPlaceHolder = false,
      controlFlowGraph = ControlFlowGraph.toControlFlowGraph(interpreter.brboProgram)
    )
    val classifierResults = tables.toProgramTables.generateClassifiers(debugMode = false)
    val transformation = classifierResults.toTransformation.map({
      case (command, ast) =>
        s"Transform ${command.asInstanceOf[Command].printToIR()} into:\n${ast.printToC(0)}"
    }).toList.sorted.mkString("\n\n")
    StringCompare.ignoreWhitespaces(
      transformation,
      """Transform resetPlaceHolder_1(); into:
        |{
        |  if ((i < 0) || (i == 0))
        |  {
        |    ;
        |  }
        |  else
        |  {
        |    if ((i < 1) || (i == 1))
        |    {
        |      if (S1 < R1)
        |      {
        |        S1 = R1;
        |      }
        |      else
        |      {
        |        ;
        |      }
        |      R1 = 0;
        |      C1 = C1 + 1;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |  }
        |  if ((i < 1) || (i == 1))
        |  {
        |    ;
        |  }
        |  else
        |  {
        |    if ((i < 2) || (i == 2))
        |    {
        |      if (S0 < R0)
        |      {
        |        S0 = R0;
        |      }
        |      else
        |      {
        |        ;
        |      }
        |      R0 = 0;
        |      C0 = C0 + 1;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |  }
        |}
        |
        |Transform use R0 1011 into:
        |if ((i < 1) || (i == 1))
        |{
        |  R0 = R0 + 1011;
        |}
        |else
        |{
        |  if ((i < 2) || (i == 2))
        |  {
        |    R1 = R1 + 1011;
        |  }
        |  else
        |  {
        |    R0 = R0 + 1011;
        |  }
        |}
        |
        |Transform use R0 2011 into:
        |R1 = R1 + 2011;
        |""".stripMargin,
      "Generating transformations failed"
    )
  }
}

object ClassifierUnitTest {
  def generateGroups(trace: Trace, numberOfGroups: Int): Map[GroupID, Group] = {
    val costNodeIndices = trace.costTraceAssociation.indexMap.keys.toList.sorted
    generateGroups(costNodeIndices, numberOfGroups)
  }

  private def generateGroups(indices: List[Int], numberOfGroups: Int): Map[GroupID, Group] = {
    assert(numberOfGroups > 0)
    Range(0, numberOfGroups).map({
      groupID =>
        val groupIndices = indices.filter(index => index % numberOfGroups == groupID)
        val numberOfSegmentsPerGroup = 1
        val segments = groupIndices.zipWithIndex.groupBy({
          case (_, index) => index % (groupIndices.size / numberOfSegmentsPerGroup)
        }).values.map({
          list => Segment(list.map({ case (index, _) => index }))
        }).toList.sortWith({ case (s1, s2) => s1.lessThan(s2) })
        (GroupID(groupID), Group(segments))
    }).toMap
  }

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
       |    // use(0, 88);
       |    // use(0, 89);
       |  }
       |$functionDefinitions
       |}""".stripMargin

  private def addUseTableRow(table: UseTable, features: List[Int], label: Int): Unit = {
    table.addRow(features.map(n => Number(n)), UseLabel(GroupID(label)))
  }

  private def addResetTableRow(table: ResetTable, features: List[Int], label: Boolean): Unit = {
    table.addRow(features.map(n => Number(n)), ResetLabel(label))
  }

  private val useTable = {
    val table = new UseTable(List(Identifier("x", INT), Identifier("y", INT)))
    addUseTableRow(table, features = List(0, 1), label = 1)
    addUseTableRow(table, features = List(0, 2), label = 1)
    addUseTableRow(table, features = List(0, 3), label = 1)
    addUseTableRow(table, features = List(0, 4), label = 2)
    table
  }

  private val resetTable = {
    val table = new ResetTable(List(Identifier("x", INT), Identifier("y", INT)))
    addResetTableRow(table, features = List(0, 1), label = true)
    addResetTableRow(table, features = List(0, 2), label = false)
    addResetTableRow(table, features = List(0, 3), label = true)
    addResetTableRow(table, features = List(0, 4), label = false)
    table
  }

  val classifierTest: List[TestCase] = List(
    TestCase("useTable", useTable,
      """Labels: List(Label(GroupID(1)), Label(GroupID(2)))
        |Tree:
        |InterNode(id=0, threshold=3.5, featureID=1)
        |  LeafNode(id=1, classID=GroupID(1)) (if y <= 3.5)
        |  LeafNode(id=2, classID=GroupID(2)) (if y > 3.5)""".stripMargin),
    TestCase("resetTable", resetTable,
      """Labels: List(Label(false), Label(true))
        |Tree:
        |InterNode(id=0, threshold=1.5, featureID=1)
        |  LeafNode(id=1, classID=true) (if y <= 1.5)
        |  InterNode(id=2, threshold=2.5, featureID=1)
        |    LeafNode(id=3, classID=false) (if y <= 2.5)
        |    InterNode(id=4, threshold=3.5, featureID=1)
        |      LeafNode(id=5, classID=true) (if y <= 3.5)
        |      LeafNode(id=6, classID=false) (if y > 3.5) (if y > 2.5) (if y > 1.5)""".stripMargin)
  )
}
