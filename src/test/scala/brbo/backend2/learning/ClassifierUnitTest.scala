package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.learning.Classifier.{GroupID, ResetTable, UseTable}
import brbo.backend2.learning.ClassifierUnitTest.{generateGroups, loopPhase}
import brbo.backend2.learning.SegmentClustering.{Group, Segment}
import brbo.backend2.learning.SegmentClusteringUnitTest.functionDefinitions
import brbo.common.BrboType.INT
import brbo.common.ast.{Identifier, Number}
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class ClassifierUnitTest extends AnyFlatSpec {
  "Generate tables for classification" should "be correct" in {
    val program = SegmentClusteringUnitTest.toBrboProgram(loopPhase)
    val interpreter = new Interpreter(program)
    val trace = SegmentClusteringUnitTest.getTrace(loopPhase, List(Number(4)))
    val costNodeIndices = trace.costTraceAssociation.reversedIndexMap.keys.toList.sorted
    val groups1 = generateGroups(costNodeIndices, numberOfGroups = 2)
    val table1 = Classifier.generateTables(
      trace,
      Classifier.evaluateFunctionFromInterpreter(interpreter),
      groups1,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      failIfCannotFindResetPlaceHolder = false
    )
    val decompositionExpected1 =
      """Index  |        Commands         |  GroupID(0)  |  GroupID(1)  |
        |-----------------------------------------------------------------
        |     0  |      Command not exist  |              |              |
        |     1  |                     0;  |              |              |
        |     2  |            int C0 = 0;  |              |              |
        |     3  |                     0;  |              |              |
        |     4  |            int R0 = 0;  |              |              |
        |     5  |           -2147483648;  |              |              |
        |     6  |  int S0 = -2147483648;  |              |              |
        |     7  |     <resetPlaceHolder>  |              |              |
        |     8  |                     0;  |              |              |
        |     9  |             int i = 0;  |              |              |
        |    10  |                     i;  |              |              |
        |    11  |                     n;  |              |              |
        |    12  |               (i < n);  |              |              |
        |    13  |     <resetPlaceHolder>  |              |              |
        |    14  |                     i;  |              |              |
        |    15  |                     0;  |              |              |
        |    16  |              (i == 0);  |              |              |
        |    17  |           !((i == 0));  |              |              |
        |    18  |                  true;  |              |              |
        |    19  |                  2011;  |              |              |
        |    20  |        R0 = R0 + 2011;  |           0  |              |
        |    21  |                     i;  |              |              |
        |    22  |                     1;  |              |              |
        |    23  |               (i + 1);  |              |              |
        |    24  |             i = i + 1;  |              |              |
        |    25  |                     i;  |              |              |
        |    26  |                     n;  |              |              |
        |    27  |               (i < n);  |              |              |
        |    28  |     <resetPlaceHolder>  |              |              |
        |    29  |                     i;  |              |              |
        |    30  |                     0;  |              |              |
        |    31  |              (i == 0);  |              |              |
        |    32  |           !((i == 0));  |              |              |
        |    33  |                  true;  |              |              |
        |    34  |                  1011;  |              |              |
        |    35  |        R0 = R0 + 1011;  |              |           0  |
        |    36  |                     i;  |              |              |
        |    37  |                     1;  |              |              |
        |    38  |               (i + 1);  |              |              |
        |    39  |             i = i + 1;  |              |              |
        |    40  |                     i;  |              |              |
        |    41  |                     n;  |              |              |
        |    42  |               (i < n);  |              |              |
        |    43  |     <resetPlaceHolder>  |              |              |
        |    44  |                     i;  |              |              |
        |    45  |                     0;  |              |              |
        |    46  |              (i == 0);  |              |              |
        |    47  |           !((i == 0));  |              |              |
        |    48  |                  true;  |              |              |
        |    49  |                  1011;  |              |              |
        |    50  |        R0 = R0 + 1011;  |           1  |              |
        |    51  |                     i;  |              |              |
        |    52  |                     1;  |              |              |
        |    53  |               (i + 1);  |              |              |
        |    54  |             i = i + 1;  |              |              |
        |    55  |                     i;  |              |              |
        |    56  |                     n;  |              |              |
        |    57  |               (i < n);  |              |              |
        |    58  |     <resetPlaceHolder>  |              |              |
        |    59  |                     i;  |              |              |
        |    60  |                     0;  |              |              |
        |    61  |              (i == 0);  |              |              |
        |    62  |           !((i == 0));  |              |              |
        |    63  |                  true;  |              |              |
        |    64  |                  1011;  |              |              |
        |    65  |        R0 = R0 + 1011;  |              |           1  |
        |    66  |                     i;  |              |              |
        |    67  |                     1;  |              |              |
        |    68  |               (i + 1);  |              |              |
        |    69  |             i = i + 1;  |              |              |
        |    70  |                     i;  |              |              |
        |    71  |                     n;  |              |              |
        |    72  |               (i < n);  |              |              |
        |    73  |                  true;  |              |              |
        |    74  |                    88;  |              |              |
        |    75  |          R0 = R0 + 88;  |              |           2  |
        |    76  |                  true;  |              |              |
        |    77  |                    89;  |              |              |
        |    78  |          R0 = R0 + 89;  |           2  |              |""".stripMargin
    StringCompare.ignoreWhitespaces(SegmentClustering.printDecomposition(trace, groups1), decompositionExpected1, "decomposition 1 failed")

    val expected1 =
      """Tables:
        |Features: i, n
        |Location: <resetPlaceHolder> (index=13) ->
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
        |Location: <resetPlaceHolder> (index=28) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 1  |  4  |   true  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 1  |  4  |  false  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=43) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 2  |  4  |  false  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 2  |  4  |   true  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=58) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 3  |  4  |   true  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 3  |  4  |  false  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=7) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        |    |  4  |  false  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        |    |  4  |  false  |
        |************************************************************
        |
        |Location: use R0 1011 (index=35) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 1  |  4  |  GroupID(1)  |
        |************************************************************
        |
        |Location: use R0 1011 (index=50) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 2  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |Location: use R0 1011 (index=65) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 3  |  4  |  GroupID(1)  |
        |************************************************************
        |
        |Location: use R0 2011 (index=20) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 0  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |Location: use R0 88 (index=75) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 4  |  4  |  GroupID(1)  |
        |************************************************************
        |
        |Location: use R0 89 (index=78) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 4  |  4  |  GroupID(0)  |
        |""".stripMargin
    StringCompare.ignoreWhitespaces(table1.print(), expected1, "loopPhase failed: Multiple groups")

    val groups2 = Map(groups1.head)
    val table2 = Classifier.generateTables(
      trace,
      Classifier.evaluateFunctionFromInterpreter(interpreter),
      groups2,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      failIfCannotFindResetPlaceHolder = false
    )
    val decompositionExpected2 =
      """Index  |        Commands         |  GroupID(0)  |
        |--------------------------------------------------
        |     0  |      Command not exist  |              |
        |     1  |                     0;  |              |
        |     2  |            int C0 = 0;  |              |
        |     3  |                     0;  |              |
        |     4  |            int R0 = 0;  |              |
        |     5  |           -2147483648;  |              |
        |     6  |  int S0 = -2147483648;  |              |
        |     7  |     <resetPlaceHolder>  |              |
        |     8  |                     0;  |              |
        |     9  |             int i = 0;  |              |
        |    10  |                     i;  |              |
        |    11  |                     n;  |              |
        |    12  |               (i < n);  |              |
        |    13  |     <resetPlaceHolder>  |              |
        |    14  |                     i;  |              |
        |    15  |                     0;  |              |
        |    16  |              (i == 0);  |              |
        |    17  |           !((i == 0));  |              |
        |    18  |                  true;  |              |
        |    19  |                  2011;  |              |
        |    20  |        R0 = R0 + 2011;  |           0  |
        |    21  |                     i;  |              |
        |    22  |                     1;  |              |
        |    23  |               (i + 1);  |              |
        |    24  |             i = i + 1;  |              |
        |    25  |                     i;  |              |
        |    26  |                     n;  |              |
        |    27  |               (i < n);  |              |
        |    28  |     <resetPlaceHolder>  |              |
        |    29  |                     i;  |              |
        |    30  |                     0;  |              |
        |    31  |              (i == 0);  |              |
        |    32  |           !((i == 0));  |              |
        |    33  |                  true;  |              |
        |    34  |                  1011;  |              |
        |    35  |        R0 = R0 + 1011;  |              |
        |    36  |                     i;  |              |
        |    37  |                     1;  |              |
        |    38  |               (i + 1);  |              |
        |    39  |             i = i + 1;  |              |
        |    40  |                     i;  |              |
        |    41  |                     n;  |              |
        |    42  |               (i < n);  |              |
        |    43  |     <resetPlaceHolder>  |              |
        |    44  |                     i;  |              |
        |    45  |                     0;  |              |
        |    46  |              (i == 0);  |              |
        |    47  |           !((i == 0));  |              |
        |    48  |                  true;  |              |
        |    49  |                  1011;  |              |
        |    50  |        R0 = R0 + 1011;  |           1  |
        |    51  |                     i;  |              |
        |    52  |                     1;  |              |
        |    53  |               (i + 1);  |              |
        |    54  |             i = i + 1;  |              |
        |    55  |                     i;  |              |
        |    56  |                     n;  |              |
        |    57  |               (i < n);  |              |
        |    58  |     <resetPlaceHolder>  |              |
        |    59  |                     i;  |              |
        |    60  |                     0;  |              |
        |    61  |              (i == 0);  |              |
        |    62  |           !((i == 0));  |              |
        |    63  |                  true;  |              |
        |    64  |                  1011;  |              |
        |    65  |        R0 = R0 + 1011;  |              |
        |    66  |                     i;  |              |
        |    67  |                     1;  |              |
        |    68  |               (i + 1);  |              |
        |    69  |             i = i + 1;  |              |
        |    70  |                     i;  |              |
        |    71  |                     n;  |              |
        |    72  |               (i < n);  |              |
        |    73  |                  true;  |              |
        |    74  |                    88;  |              |
        |    75  |          R0 = R0 + 88;  |              |
        |    76  |                  true;  |              |
        |    77  |                    89;  |              |
        |    78  |          R0 = R0 + 89;  |           2  |""".stripMargin
    StringCompare.ignoreWhitespaces(SegmentClustering.printDecomposition(trace, groups2), decompositionExpected2, "decomposition 2 failed")
    val expected2 =
      """Tables:
        |Features: i, n
        |Location: <resetPlaceHolder> (index=13) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=28) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 1  |  4  |   true  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=43) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 2  |  4  |  false  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=58) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 3  |  4  |   true  |
        |************************************************************
        |
        |Location: <resetPlaceHolder> (index=7) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        |    |  4  |  false  |
        |************************************************************
        |
        |Location: use R0 1011 (index=35) ->
        |Use table:
        | i  |  n  |    Label    |
        |-------------------------
        | 1  |  4  |  NoneGroup  |
        |************************************************************
        |
        |Location: use R0 1011 (index=50) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 2  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |Location: use R0 1011 (index=65) ->
        |Use table:
        | i  |  n  |    Label    |
        |-------------------------
        | 3  |  4  |  NoneGroup  |
        |************************************************************
        |
        |Location: use R0 2011 (index=20) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 0  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |Location: use R0 88 (index=75) ->
        |Use table:
        | i  |  n  |    Label    |
        |-------------------------
        | 4  |  4  |  NoneGroup  |
        |************************************************************
        |
        |Location: use R0 89 (index=78) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 4  |  4  |  GroupID(0)  |
        |""".stripMargin
    StringCompare.ignoreWhitespaces(table2.print(), expected2, "loopPhase failed: Single group")

    // TODO: More tests
  }

  "Learning classifiers from tables" should "be correct" in {
    ClassifierUnitTest.classifierTest.foreach({
      testCase =>
        val result = testCase.input match {
          case table: ResetTable => Classifier.classify(table, debugMode = false)
          case table: UseTable => Classifier.classify(table, debugMode = false)
        }
        StringCompare.ignoreWhitespaces(result.print(indent = 0), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object ClassifierUnitTest {
  def generateGroups(indices: List[Int], numberOfGroups: Int): Map[GroupID, Group] = {
    assert(numberOfGroups > 0)
    Range(0, numberOfGroups).map({
      groupID =>
        val groupIndices = indices.filter(index => index % numberOfGroups == groupID)
        val segments = groupIndices.zipWithIndex.groupBy({
          case (_, index) => index / (groupIndices.size / 3)
        }).values.map({
          list => Segment(list.map({ case (index, _) => index }))
        }).toList.sortWith({ case (s1, s2) => s1.lessThanOrEqualTo(s2) })
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
       |    use(0, 88);
       |    use(0, 89);
       |  }
       |$functionDefinitions
       |}""".stripMargin

  private def addUseTableRow(table: UseTable, features: List[Int], label: Int): Unit = {
    table.addRow(features.map(n => Number(n)), GroupID(label))
  }

  private def addResetTableRow(table: ResetTable, features: List[Int], label: Boolean): Unit = {
    table.addRow(features.map(n => Number(n)), label)
  }

  private val useTable = {
    val table = new UseTable(List("x", "y"))
    addUseTableRow(table, features = List(0, 1), label = 1)
    addUseTableRow(table, features = List(0, 2), label = 1)
    addUseTableRow(table, features = List(0, 3), label = 1)
    addUseTableRow(table, features = List(0, 4), label = 2)
    table
  }

  private val resetTable = {
    val table = new ResetTable(List("x", "y"))
    addResetTableRow(table, features = List(0, 1), label = true)
    addResetTableRow(table, features = List(0, 2), label = false)
    addResetTableRow(table, features = List(0, 3), label = true)
    addResetTableRow(table, features = List(0, 4), label = false)
    table
  }

  val classifierTest: List[TestCase] = List(
    TestCase("useTable", useTable,
      """InterNode(id=0, threshold=3.5, featureID=1)
        |  LeafNode(id=1, classID=0) (if feature[1] <= 3.5)
        |  LeafNode(id=2, classID=1) (if feature[1] > 3.5)""".stripMargin),
    TestCase("resetTable", resetTable,
      """InterNode(id=0, threshold=1.5, featureID=1)
        |  LeafNode(id=1, classID=1) (if feature[1] <= 1.5)
        |  InterNode(id=2, threshold=2.5, featureID=1)
        |    LeafNode(id=3, classID=0) (if feature[1] <= 2.5)
        |    InterNode(id=4, threshold=3.5, featureID=1)
        |      LeafNode(id=5, classID=1) (if feature[1] <= 3.5)
        |      LeafNode(id=6, classID=0) (if feature[1] > 3.5) (if feature[1] > 2.5) (if feature[1] > 1.5)""".stripMargin)
  )
}
