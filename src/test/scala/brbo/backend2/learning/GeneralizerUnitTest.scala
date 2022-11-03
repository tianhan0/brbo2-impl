package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.learning.Generalizer.GroupID
import brbo.backend2.learning.GeneralizerUnitTest.{loopPhase, randomlyGenerateGroups}
import brbo.backend2.learning.SegmentClustering.{Group, Segment}
import brbo.backend2.learning.SegmentClusteringUnitTest.functionDefinitions
import brbo.common.BrboType.INT
import brbo.common.ast.{Identifier, Number}
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class GeneralizerUnitTest extends AnyFlatSpec {
  "Generate tables for classification" should "be correct" in {
    val program = SegmentClusteringUnitTest.toBrboProgram(loopPhase)
    val interpreter = new Interpreter(program)
    val trace = SegmentClusteringUnitTest.getTrace(loopPhase, List(Number(4)))
    val costNodeIndices = trace.costTraceAssociation.reversedIndexMap.keys.toList.sorted
    val groups = randomlyGenerateGroups(costNodeIndices, numberOfGroups = 2)
    val table1 = Generalizer.generateTables(
      trace,
      Generalizer.evaluateFromInterpreter(interpreter),
      groups,
      features = List(Identifier("i", INT), Identifier("n", INT)),
      failIfCannotFindResetPlaceHolder = false
    )
    val decompositionExpected1 =
      """Commands         |  GroupID(0)  |  GroupID(1)  |
        |-------------------------------------------------------
        |     Command not exist  |              |              |
        |                    0;  |              |              |
        |           int C0 = 0;  |              |              |
        |                    0;  |              |              |
        |           int R0 = 0;  |              |              |
        |          -2147483648;  |              |              |
        | int S0 = -2147483648;  |              |              |
        |    <resetPlaceHolder>  |              |              |
        |                    0;  |              |              |
        |            int i = 0;  |              |              |
        |                    i;  |              |              |
        |                    n;  |              |              |
        |              (i < n);  |              |              |
        |    <resetPlaceHolder>  |              |              |
        |                    i;  |              |              |
        |                    0;  |              |              |
        |             (i == 0);  |              |              |
        |          !((i == 0));  |              |              |
        |                 true;  |              |              |
        |                 2011;  |              |              |
        |       R0 = R0 + 2011;  |           2  |              |
        |                    i;  |              |              |
        |                    1;  |              |              |
        |              (i + 1);  |              |              |
        |            i = i + 1;  |              |              |
        |                    i;  |              |              |
        |                    n;  |              |              |
        |              (i < n);  |              |              |
        |    <resetPlaceHolder>  |              |              |
        |                    i;  |              |              |
        |                    0;  |              |              |
        |             (i == 0);  |              |              |
        |          !((i == 0));  |              |              |
        |                 true;  |              |              |
        |                 1011;  |              |              |
        |       R0 = R0 + 1011;  |              |           2  |
        |                    i;  |              |              |
        |                    1;  |              |              |
        |              (i + 1);  |              |              |
        |            i = i + 1;  |              |              |
        |                    i;  |              |              |
        |                    n;  |              |              |
        |              (i < n);  |              |              |
        |    <resetPlaceHolder>  |              |              |
        |                    i;  |              |              |
        |                    0;  |              |              |
        |             (i == 0);  |              |              |
        |          !((i == 0));  |              |              |
        |                 true;  |              |              |
        |                 1011;  |              |              |
        |       R0 = R0 + 1011;  |           1  |              |
        |                    i;  |              |              |
        |                    1;  |              |              |
        |              (i + 1);  |              |              |
        |            i = i + 1;  |              |              |
        |                    i;  |              |              |
        |                    n;  |              |              |
        |              (i < n);  |              |              |
        |    <resetPlaceHolder>  |              |              |
        |                    i;  |              |              |
        |                    0;  |              |              |
        |             (i == 0);  |              |              |
        |          !((i == 0));  |              |              |
        |                 true;  |              |              |
        |                 1011;  |              |              |
        |       R0 = R0 + 1011;  |              |           1  |
        |                    i;  |              |              |
        |                    1;  |              |              |
        |              (i + 1);  |              |              |
        |            i = i + 1;  |              |              |
        |                    i;  |              |              |
        |                    n;  |              |              |
        |              (i < n);  |              |              |
        |                 true;  |              |              |
        |                   88;  |              |              |
        |         R0 = R0 + 88;  |              |           0  |
        |                 true;  |              |              |
        |                   89;  |              |              |
        |         R0 = R0 + 89;  |           0  |              |""".stripMargin
    StringCompare.ignoreWhitespaces(SegmentClustering.printDecomposition(trace, groups), decompositionExpected1, "decomposition 1 failed")

    // TODO: We should be able to find reset place holders!
    val expected1 =
      """Tables:
        |Features: i, n
        |Location(<resetPlaceHolder>) ->
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
        |Location(<resetPlaceHolder>) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        | 1  |  4  |  false  |
        | 2  |  4  |  false  |
        | 3  |  4  |  false  |
        |========================================
        |Reset table: GroupID(1) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        | 1  |  4  |  false  |
        | 2  |  4  |  false  |
        | 3  |  4  |  false  |
        |************************************************************
        |
        |Location(use R0 1011) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 1  |  4  |  GroupID(1)  |
        | 2  |  4  |  GroupID(0)  |
        | 3  |  4  |  GroupID(1)  |
        |************************************************************
        |
        |Location(use R0 2011) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 0  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |Location(use R0 88) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 4  |  4  |  GroupID(1)  |
        |************************************************************
        |
        |Location(use R0 89) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 4  |  4  |  GroupID(0)  |""".stripMargin
    StringCompare.ignoreWhitespaces(table1.print(), expected1, "loopPhase failed: Multiple groups")

    val table2 = Generalizer.generateTables(
      trace,
      Generalizer.evaluateFromInterpreter(interpreter),
      Map(groups.head),
      features = List(Identifier("i", INT), Identifier("n", INT)),
      failIfCannotFindResetPlaceHolder = false
    )
    val expected2 =
      """Tables:
        |Features: i, n
        |Location(<resetPlaceHolder>) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        |    |  4  |  false  |
        |************************************************************
        |
        |Location(<resetPlaceHolder>) ->
        |Reset table: GroupID(0) ->
        | i  |  n  |  Label  |
        |---------------------
        | 0  |  4  |  false  |
        | 1  |  4  |  false  |
        | 2  |  4  |  false  |
        | 3  |  4  |  false  |
        |************************************************************
        |
        |Location(use R0 1011) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 1  |  4  |   NoneGroup  |
        | 2  |  4  |  GroupID(0)  |
        | 3  |  4  |   NoneGroup  |
        |************************************************************
        |
        |Location(use R0 2011) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 0  |  4  |  GroupID(0)  |
        |************************************************************
        |
        |Location(use R0 88) ->
        |Use table:
        | i  |  n  |    Label    |
        |-------------------------
        | 4  |  4  |  NoneGroup  |
        |************************************************************
        |
        |Location(use R0 89) ->
        |Use table:
        | i  |  n  |    Label     |
        |--------------------------
        | 4  |  4  |  GroupID(0)  |
        |""".stripMargin
    StringCompare.ignoreWhitespaces(table2.print(), expected2, "loopPhase failed: Single group")

    // TODO: More tests
  }
}

object GeneralizerUnitTest {
  def randomlyGenerateGroups(indices: List[Int], numberOfGroups: Int): Map[GroupID, Group] = {
    assert(numberOfGroups > 0)
    Range(0, numberOfGroups).map({
      groupID =>
        val groupIndices = indices.filter(index => index % numberOfGroups == groupID)
        val segments = groupIndices.zipWithIndex.groupBy({
          case (_, index) => index / (groupIndices.size / 3)
        }).values.map({
          list => Segment(list.map({ case (index, _) => index }))
        }).toList
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
}
