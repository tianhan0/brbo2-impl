package brbo.common.cfg

import brbo.TestCase
import brbo.common.BrboType.{INT, VOID}
import brbo.common.PreDefinedFunctions
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.common.ast._
import brbo.common.cfg.ControlFlowGraphUnitTest.{test05, test05InnerLoopCondition, test05InnerLoopContinue}
import brbo.common.string.StringCompare
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.flatspec.AnyFlatSpec

class ControlFlowGraphUnitTest extends AnyFlatSpec {
  val logger: Logger = LogManager.getLogger(classOf[ControlFlowGraphUnitTest])

  "Generating control flow graphs" should "be correct" in {
    ControlFlowGraphUnitTest.testCases.foreach({
      testCase =>
        val controlFlowGraph = ControlFlowGraph.toControlFlowGraph(testCase.input.asInstanceOf[BrboProgram])
        controlFlowGraph.printPDF()
        val dotRepresentation = ControlFlowGraph.exportToDOT(controlFlowGraph.jgraphtGraph)
        StringCompare.ignoreWhitespaces(dotRepresentation, testCase.expectedOutput, s"${testCase.name} failed!")
    })
  }

  "Equality tests between CFG Nodes" should "be correct" in {
    val empty1 = Empty()
    val empty2 = Empty()
    val node1 = CFGNode(empty1, None)
    val node2 = CFGNode(empty1, None)
    val node3 = CFGNode(empty2, None)
    val set1 = List(node1, node2).distinct
    val set2 = List(node2, node3).distinct
    StringCompare.ignoreWhitespaces(set1.toString(), """List((-1) [Empty Node] [fun `NONE!`])""")
    StringCompare.ignoreWhitespaces(set2.toString(), """List((-1) [Empty Node] [fun `NONE!`], (-1) [Empty Node] [fun `NONE!`])""")
  }

  "Finding the closest dominator" should "be correct" in {
    def branchingHeadPredicate(node: CFGNode): Boolean = node.command.isInstanceOf[BranchingHead]

    def lessThanPredicate(node: CFGNode): Boolean = node.command.isInstanceOf[LessThan]

    def assignmentPredicate(node: CFGNode): Boolean = node.command.isInstanceOf[Assignment]

    val controlFlowGraph = ControlFlowGraph.toControlFlowGraph(test05)
    val nodes = controlFlowGraph.nodesFromCommands(Set(test05InnerLoopCondition, test05InnerLoopContinue))
    val nodesString = s"{${nodes.map(n => n.printToIR()).mkString(", ")}}"
    val closestBranchingHead = controlFlowGraph.closestDominator(nodes, branchingHeadPredicate)
    StringCompare.ignoreWhitespaces(closestBranchingHead.get.printToIR(), "(11) [Branch Head]",
      s"Finding the closest dominating BranchingHead failed for nodes $nodesString")
    val closestLessThan = controlFlowGraph.closestDominator(nodes, lessThanPredicate)
    StringCompare.ignoreWhitespaces(closestLessThan.get.printToIR(), "(8) (i < 7)",
      s"Finding the closest dominating LessThan failed")

    val (reversedGraph, reversedRoot) = ControlFlowGraph.reverseGraph(controlFlowGraph)
    // println(reversedGraph.toString)
    // println(nodesString)
    val closestPostBranchingHead = ControlFlowGraph.closestDominator(
      graph = reversedGraph,
      entryNode = reversedRoot,
      nodes,
      predicate = branchingHeadPredicate
    )
    StringCompare.ignoreWhitespaces(closestPostBranchingHead.get.printToIR(), "(3) [Branch Head]",
      s"Finding the closest post-dominating BranchingHead failed for nodes $nodesString")

    val closestPostAssignment = ControlFlowGraph.closestDominator(
      graph = reversedGraph,
      entryNode = reversedRoot,
      nodes,
      predicate = assignmentPredicate
    )
    StringCompare.ignoreWhitespaces(closestPostAssignment.get.printToIR(), "(16) i = 12;",
      s"Finding the closest post-dominating Assignment failed for nodes $nodesString")
  }

  "Deep copy a graph" should "be correct" in {
    val controlFlowGraph = ControlFlowGraph.toControlFlowGraph(test05)
    val (entry, exit) = {
      val graph = controlFlowGraph.cfgs.head._2
      (graph.root, graph.exits.head)
    }
    val (copiedGraph, root) = ControlFlowGraph.deepCopySimpleDirectedGraph(
      graph = controlFlowGraph.jgraphtGraph,
      entry = entry,
      exit = exit,
      copyNode = CFGNode.copyNodeOnly,
      reverse = false
    )
    val copiedGraphDot = ControlFlowGraph.exportToDOT(copiedGraph)
    ControlFlowGraph.printDotToPDF(s"${test05.className}_nonreversed", dotFileContents = copiedGraphDot)
    StringCompare.ignoreWhitespaces(s"root: ${root.printToIR()}\n$copiedGraphDot",
      """root: (2) int i = 0;
        |strict digraph G {
        |  1 [ label="(2) int i = 0;" shape="rectangle" ];
        |  2 [ label="(3) [Branch Head]" shape="oval" ];
        |  3 [ label="(4) (i < 5)" shape="diamond" ];
        |  4 [ label="(5) !((i < 5))" shape="diamond" ];
        |  5 [ label="(6) [Loop Exit]" shape="oval" ];
        |  6 [ label="(1) [Function Exit]" shape="oval" ];
        |  7 [ label="(7) [Branch Head]" shape="oval" ];
        |  8 [ label="(8) (i < 7)" shape="diamond" ];
        |  9 [ label="(9) !((i < 7))" shape="diamond" ];
        |  10 [ label="(10) [Loop Exit]" shape="oval" ];
        |  11 [ label="(16) i = 12;" shape="rectangle" ];
        |  12 [ label="(11) [Branch Head]" shape="oval" ];
        |  13 [ label="(12) (i < 10)" shape="diamond" ];
        |  14 [ label="(13) !((i < 10))" shape="diamond" ];
        |  15 [ label="(15) continue;" shape="rectangle" ];
        |  16 [ label="(14) break;" shape="rectangle" ];
        |  1 -> 2 [ label="1.0" ];
        |  2 -> 3 [ label="1.0" ];
        |  2 -> 4 [ label="1.0" ];
        |  4 -> 5 [ label="1.0" ];
        |  5 -> 6 [ label="1.0" ];
        |  3 -> 7 [ label="1.0" ];
        |  7 -> 8 [ label="1.0" ];
        |  7 -> 9 [ label="1.0" ];
        |  9 -> 10 [ label="1.0" ];
        |  10 -> 11 [ label="1.0" ];
        |  11 -> 2 [ label="1.0" ];
        |  8 -> 12 [ label="1.0" ];
        |  12 -> 13 [ label="1.0" ];
        |  12 -> 14 [ label="1.0" ];
        |  14 -> 15 [ label="1.0" ];
        |  15 -> 7 [ label="1.0" ];
        |  13 -> 16 [ label="1.0" ];
        |  16 -> 10 [ label="1.0" ];
        |}""".stripMargin)

    val (reversedCopiedGraph, reversedRoot) = ControlFlowGraph.deepCopySimpleDirectedGraph(
      graph = controlFlowGraph.jgraphtGraph,
      entry = entry,
      exit = exit,
      copyNode = CFGNode.copyNodeOnly,
      reverse = true
    )
    val reversedCopiedGraphDot = ControlFlowGraph.exportToDOT(reversedCopiedGraph)
    ControlFlowGraph.printDotToPDF(s"${test05.className}_reversed", dotFileContents = reversedCopiedGraphDot)
    StringCompare.ignoreWhitespaces(s"root: ${reversedRoot.printToIR()}\n$reversedCopiedGraphDot",
      """root: (1) [Function Exit]
        |strict digraph G {
        |  1 [ label="(2) int i = 0;" shape="rectangle" ];
        |  2 [ label="(3) [Branch Head]" shape="oval" ];
        |  3 [ label="(4) (i < 5)" shape="diamond" ];
        |  4 [ label="(5) !((i < 5))" shape="diamond" ];
        |  5 [ label="(6) [Loop Exit]" shape="oval" ];
        |  6 [ label="(1) [Function Exit]" shape="oval" ];
        |  7 [ label="(7) [Branch Head]" shape="oval" ];
        |  8 [ label="(8) (i < 7)" shape="diamond" ];
        |  9 [ label="(9) !((i < 7))" shape="diamond" ];
        |  10 [ label="(10) [Loop Exit]" shape="oval" ];
        |  11 [ label="(16) i = 12;" shape="rectangle" ];
        |  12 [ label="(11) [Branch Head]" shape="oval" ];
        |  13 [ label="(12) (i < 10)" shape="diamond" ];
        |  14 [ label="(13) !((i < 10))" shape="diamond" ];
        |  15 [ label="(15) continue;" shape="rectangle" ];
        |  16 [ label="(14) break;" shape="rectangle" ];
        |  2 -> 1 [ label="1.0" ];
        |  3 -> 2 [ label="1.0" ];
        |  4 -> 2 [ label="1.0" ];
        |  5 -> 4 [ label="1.0" ];
        |  6 -> 5 [ label="1.0" ];
        |  7 -> 3 [ label="1.0" ];
        |  8 -> 7 [ label="1.0" ];
        |  9 -> 7 [ label="1.0" ];
        |  10 -> 9 [ label="1.0" ];
        |  11 -> 10 [ label="1.0" ];
        |  2 -> 11 [ label="1.0" ];
        |  12 -> 8 [ label="1.0" ];
        |  13 -> 12 [ label="1.0" ];
        |  14 -> 12 [ label="1.0" ];
        |  15 -> 14 [ label="1.0" ];
        |  7 -> 15 [ label="1.0" ];
        |  16 -> 13 [ label="1.0" ];
        |  10 -> 16 [ label="1.0" ];
        |}""".stripMargin)
  }
}

object ControlFlowGraphUnitTest {
  private val test01 = {
    val i = Identifier("i", INT)
    val variableDeclaration = VariableDeclaration(i, Number(0))
    val loop = Loop(LessThan(i, Number(10)), Block(List(Assignment(i, Addition(i, Number(1))), Break())))
    val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)), Set())
    BrboProgram("test01", packageName = None, main, PreDefinedFunctions.functionCRepresentations, Nil)
  }
  private val test01Expected =
    """strict digraph G {
      |  1 [ label="(1) [Function Exit]" shape="oval" ];
      |  2 [ label="(2) int i = 0;" shape="rectangle" ];
      |  3 [ label="(3) [Branch Head]" shape="oval" ];
      |  4 [ label="(4) (i < 10)" shape="diamond" ];
      |  5 [ label="(5) !((i < 10))" shape="diamond" ];
      |  6 [ label="(6) [Loop Exit]" shape="oval" ];
      |  7 [ label="(7) i = i + 1;" shape="rectangle" ];
      |  8 [ label="(8) break;" shape="rectangle" ];
      |  9 [ label="(9) [Function Exit]" shape="oval" ];
      |  10 [ label="(10) int x = ndInt();" shape="rectangle" ];
      |  11 [ label="(11) [Branch Head]" shape="oval" ];
      |  12 [ label="(12) (!((x < 0)) && !((x == 0)))" shape="diamond" ];
      |  13 [ label="(13) !((!((x < 0)) && !((x == 0))))" shape="diamond" ];
      |  14 [ label="(14) return true;" shape="rectangle" ];
      |  15 [ label="(15) return false;" shape="rectangle" ];
      |  16 [ label="(16) [Function Exit]" shape="oval" ];
      |  17 [ label="(17) assume(((lower < x) || (lower == x)) && ((x < upper) || (x == upper)))" shape="diamond" ];
      |  18 [ label="(18) [Function Exit]" shape="oval" ];
      |  19 [ label="(19) int x = ndInt();" shape="rectangle" ];
      |  20 [ label="(20) assume(((lower < x) || (lower == x)) && ((x < upper) || (x == upper)))" shape="diamond" ];
      |  21 [ label="(21) return x;" shape="rectangle" ];
      |  22 [ label="(22) [Function Exit]" shape="oval" ];
      |  23 [ label="(23) return __VERIFIER_nondet_int();" shape="rectangle" ];
      |  24 [ label="(24) [Function Exit]" shape="oval" ];
      |  25 [ label="(25) [Branch Head]" shape="oval" ];
      |  26 [ label="(26) !(cond)" shape="diamond" ];
      |  27 [ label="(27) !(!(cond))" shape="diamond" ];
      |  28 [ label="(28) abort()" shape="diamond" ];
      |  29 [ label="(29) ;" shape="rectangle" ];
      |  30 [ label="(30) [Function Exit]" shape="oval" ];
      |  31 [ label="(31) [Branch Head]" shape="oval" ];
      |  32 [ label="(32) !(cond)" shape="diamond" ];
      |  33 [ label="(33) !(!(cond))" shape="diamond" ];
      |  34 [ label="(34) ERROR: __VERIFIER_error();" shape="rectangle" ];
      |  35 [ label="(35) ;" shape="rectangle" ];
      |  36 [ label="(36) return;" shape="rectangle" ];
      |  8 -> 6 [ label="0.0" ];
      |  7 -> 8 [ label="0.0" ];
      |  3 -> 4 [ label="1.0" ];
      |  3 -> 5 [ label="-1.0" ];
      |  4 -> 7 [ label="0.0" ];
      |  5 -> 6 [ label="0.0" ];
      |  2 -> 3 [ label="0.0" ];
      |  6 -> 1 [ label="0.0" ];
      |  14 -> 9 [ label="0.0" ];
      |  15 -> 9 [ label="0.0" ];
      |  11 -> 12 [ label="1.0" ];
      |  11 -> 13 [ label="-1.0" ];
      |  12 -> 14 [ label="0.0" ];
      |  13 -> 15 [ label="0.0" ];
      |  10 -> 11 [ label="0.0" ];
      |  17 -> 16 [ label="0.0" ];
      |  21 -> 18 [ label="0.0" ];
      |  19 -> 20 [ label="0.0" ];
      |  20 -> 21 [ label="0.0" ];
      |  23 -> 22 [ label="0.0" ];
      |  25 -> 26 [ label="1.0" ];
      |  25 -> 27 [ label="-1.0" ];
      |  26 -> 28 [ label="0.0" ];
      |  27 -> 29 [ label="0.0" ];
      |  28 -> 24 [ label="0.0" ];
      |  29 -> 24 [ label="0.0" ];
      |  31 -> 32 [ label="1.0" ];
      |  31 -> 33 [ label="-1.0" ];
      |  32 -> 34 [ label="0.0" ];
      |  33 -> 35 [ label="0.0" ];
      |  36 -> 30 [ label="0.0" ];
      |  34 -> 36 [ label="0.0" ];
      |  35 -> 36 [ label="0.0" ];
      |}""".stripMargin

  private val test02 = {
    val i = Identifier("i", INT)
    val variableDeclaration = VariableDeclaration(i, Number(0))
    val loop = Loop(LessThan(i, Number(10)), Block(List(Continue(), Assignment(i, Addition(i, Number(1))))))
    val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)), Set())
    BrboProgram("test02", packageName = None, main, Nil)
  }
  private val test02Expected =
    """strict digraph G {
      |  1 [ label="(1) [Function Exit]" shape="oval" ];
      |  2 [ label="(2) int i = 0;" shape="rectangle" ];
      |  3 [ label="(3) [Branch Head]" shape="oval" ];
      |  4 [ label="(4) (i < 10)" shape="diamond" ];
      |  5 [ label="(5) !((i < 10))" shape="diamond" ];
      |  6 [ label="(6) [Loop Exit]" shape="oval" ];
      |  7 [ label="(7) continue;" shape="rectangle" ];
      |  8 [ label="(8) i = i + 1;" shape="rectangle" ];
      |  7 -> 3 [ label="0.0" ];
      |  3 -> 4 [ label="1.0" ];
      |  3 -> 5 [ label="-1.0" ];
      |  4 -> 7 [ label="0.0" ];
      |  5 -> 6 [ label="0.0" ];
      |  8 -> 3 [ label="0.0" ];
      |  2 -> 3 [ label="0.0" ];
      |  6 -> 1 [ label="0.0" ];
      |}""".stripMargin

  private val test03 = {
    val i = Identifier("i", INT)
    val variableDeclaration = VariableDeclaration(i, FunctionCallExpr("ndInt2", List(Number(0), Number(1)), INT))
    val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration)), Set())
    BrboProgram("test03", packageName = None, main, Nil)
  }
  private val test03Expected =
    """strict digraph G {
      |  1 [ label="(1) [Function Exit]" shape="oval" ];
      |  2 [ label="(2) int i = ndInt2(0, 1);" shape="rectangle" ];
      |  2 -> 1 [ label="0.0" ];
      |}""".stripMargin

  private val test04 = {
    val i = Identifier("i", INT)
    val variableDeclaration = VariableDeclaration(i, Number(0))
    val ite = {
      val condition = LessThan(i, Number(5))
      val Then = Assignment(i, Number(1))
      val Else = Assignment(i, Number(2))
      ITE(condition, Then, Else)
    }
    val assignment = Assignment(i, Number(3))
    val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, ite, assignment)), Set())
    BrboProgram("test04", packageName = None, main, Nil)
  }
  private val test04Expected =
    """strict digraph G {
      |  1 [ label="(1) [Function Exit]" shape="oval" ];
      |  2 [ label="(2) int i = 0;" shape="rectangle" ];
      |  3 [ label="(3) [Branch Head]" shape="oval" ];
      |  4 [ label="(4) (i < 5)" shape="diamond" ];
      |  5 [ label="(5) !((i < 5))" shape="diamond" ];
      |  6 [ label="(6) i = 1;" shape="rectangle" ];
      |  7 [ label="(7) i = 2;" shape="rectangle" ];
      |  8 [ label="(8) i = 3;" shape="rectangle" ];
      |  3 -> 4 [ label="1.0" ];
      |  3 -> 5 [ label="-1.0" ];
      |  4 -> 6 [ label="0.0" ];
      |  5 -> 7 [ label="0.0" ];
      |  2 -> 3 [ label="0.0" ];
      |  6 -> 8 [ label="0.0" ];
      |  7 -> 8 [ label="0.0" ];
      |  8 -> 1 [ label="0.0" ];
      |}""".stripMargin

  private val test05VariableI = Identifier("i", INT)
  private val test05InnerLoopCondition = LessThan(test05VariableI, Number(10))
  private val test05InnerLoopContinue = Continue()
  private val test05 = {
    val variableDeclaration = VariableDeclaration(test05VariableI, Number(0))
    val loop = {
      val loop = {
        val ite = ITE(test05InnerLoopCondition, Break(), test05InnerLoopContinue)
        Loop(LessThan(test05VariableI, Number(7)), ite)
      }
      val assignment = Assignment(test05VariableI, Number(12))
      Loop(LessThan(test05VariableI, Number(5)), Block(List(loop, assignment)))
    }
    val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)), Set())
    BrboProgram("test05", packageName = None, main, Nil)
  }
  private val test05Expected =
    """strict digraph G {
      |  1 [ label="(1) [Function Exit]" shape="oval" ];
      |  2 [ label="(2) int i = 0;" shape="rectangle" ];
      |  3 [ label="(3) [Branch Head]" shape="oval" ];
      |  4 [ label="(4) (i < 5)" shape="diamond" ];
      |  5 [ label="(5) !((i < 5))" shape="diamond" ];
      |  6 [ label="(6) [Loop Exit]" shape="oval" ];
      |  7 [ label="(7) [Branch Head]" shape="oval" ];
      |  8 [ label="(8) (i < 7)" shape="diamond" ];
      |  9 [ label="(9) !((i < 7))" shape="diamond" ];
      |  10 [ label="(10) [Loop Exit]" shape="oval" ];
      |  11 [ label="(11) [Branch Head]" shape="oval" ];
      |  12 [ label="(12) (i < 10)" shape="diamond" ];
      |  13 [ label="(13) !((i < 10))" shape="diamond" ];
      |  14 [ label="(14) break;" shape="rectangle" ];
      |  15 [ label="(15) continue;" shape="rectangle" ];
      |  16 [ label="(16) i = 12;" shape="rectangle" ];
      |  14 -> 10 [ label="0.0" ];
      |  15 -> 7 [ label="0.0" ];
      |  11 -> 12 [ label="1.0" ];
      |  11 -> 13 [ label="-1.0" ];
      |  12 -> 14 [ label="0.0" ];
      |  13 -> 15 [ label="0.0" ];
      |  7 -> 8 [ label="1.0" ];
      |  7 -> 9 [ label="-1.0" ];
      |  8 -> 11 [ label="0.0" ];
      |  9 -> 10 [ label="0.0" ];
      |  10 -> 16 [ label="0.0" ];
      |  3 -> 4 [ label="1.0" ];
      |  3 -> 5 [ label="-1.0" ];
      |  4 -> 7 [ label="0.0" ];
      |  5 -> 6 [ label="0.0" ];
      |  16 -> 3 [ label="0.0" ];
      |  2 -> 3 [ label="0.0" ];
      |  6 -> 1 [ label="0.0" ];
      |}""".stripMargin

  private val test06 = {
    val i = Identifier("i", INT)
    val variableDeclaration = VariableDeclaration(i, Number(0))
    val reset: Reset = Reset(1, lessThanOrEqualTo(i, Number(0)))
    val use: Use = Use(Some(1), i, greaterThan(i, Number(5)))
    val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, reset, use)), Set())
    BrboProgram("test06", packageName = None, main, Nil, Nil)
  }
  private val test06Expected =
    """strict digraph G {
      |  1 [ label="(1) [Function Exit]" shape="oval" ];
      |  2 [ label="(2) int i = 0;" shape="rectangle" ];
      |  3 [ label="(3) if ((i < 0) || (i == 0)) reset R1" shape="rectangle" ];
      |  4 [ label="(4) if (!((i < 5)) && !((i == 5))) use R1 i" shape="rectangle" ];
      |  2 -> 3 [ label="0.0" ];
      |  3 -> 4 [ label="0.0" ];
      |  4 -> 1 [ label="0.0" ];
      |}""".stripMargin

  val testCases: List[TestCase] = List[TestCase](
    TestCase("Test break", test01, test01Expected),
    TestCase("Test continue", test02, test02Expected),
    TestCase("Test function calls (There must be no edges for function calls)", test03, test03Expected),
    TestCase("Test ite", test04, test04Expected),
    TestCase("Test nested loop", test05, test05Expected),
    TestCase("Test ghost commands", test06, test06Expected),
  )
}