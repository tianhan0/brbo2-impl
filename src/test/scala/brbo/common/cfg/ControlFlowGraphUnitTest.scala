package brbo.common.cfg

import brbo.TestCase
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
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
        val dotRepresentation = controlFlowGraph.exportToDOT
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
}

object ControlFlowGraphUnitTest {
  val testCases: List[TestCase] = {
    val test01 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = Loop(LessThan(i, Number(10)), Block(List(Assignment(i, Addition(i, Number(1))), Break())))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)), Set())
      BrboProgram("test01", main, Nil, PreDefinedFunctions.allFunctionsList)
    }
    val test01Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=oval label="(3) [Branch Head]" ];
                           |  4 [ shape=diamond label="(4) (i < 10)" ];
                           |  5 [ shape=diamond label="(5) !((i < 10))" ];
                           |  6 [ shape=oval label="(6) [Loop Exit]" ];
                           |  7 [ shape=rectangle label="(7) i = i + 1;" ];
                           |  8 [ shape=rectangle label="(8) break;" ];
                           |  9 [ shape=oval label="(9) [Function Exit]" ];
                           |  10 [ shape=oval label="(10) [Branch Head]" ];
                           |  11 [ shape=diamond label="(11) !(cond)" ];
                           |  12 [ shape=diamond label="(12) !(!(cond))" ];
                           |  13 [ shape=rectangle label="(13) ERROR: __VERIFIER_error();" ];
                           |  14 [ shape=rectangle label="(14) ;" ];
                           |  15 [ shape=rectangle label="(15) return;" ];
                           |  16 [ shape=oval label="(16) [Function Exit]" ];
                           |  17 [ shape=oval label="(17) [Branch Head]" ];
                           |  18 [ shape=diamond label="(18) !(cond)" ];
                           |  19 [ shape=diamond label="(19) !(!(cond))" ];
                           |  20 [ shape=rectangle label="(20) abort();" ];
                           |  21 [ shape=rectangle label="(21) ;" ];
                           |  22 [ shape=oval label="(22) [Function Exit]" ];
                           |  23 [ shape=rectangle label="(23) return __VERIFIER_nondet_int();" ];
                           |  24 [ shape=oval label="(24) [Function Exit]" ];
                           |  25 [ shape=rectangle label="(25) int x = ndInt();" ];
                           |  26 [ shape=oval label="(26) [Branch Head]" ];
                           |  27 [ shape=diamond label="(27) (x > 0)" ];
                           |  28 [ shape=diamond label="(28) !((x > 0))" ];
                           |  29 [ shape=rectangle label="(29) return true;" ];
                           |  30 [ shape=rectangle label="(30) return false;" ];
                           |  31 [ shape=oval label="(31) [Function Exit]" ];
                           |  32 [ shape=rectangle label="(32) int x = ndInt();" ];
                           |  33 [ shape=rectangle label="(33) assume((lower <= x) && (x <= upper));" ];
                           |  34 [ shape=rectangle label="(34) return x;" ];
                           |  35 [ shape=oval label="(35) [Function Exit]" ];
                           |  36 [ shape=rectangle label="(36) assume((lower <= x) && (x <= upper));" ];
                           |  37 [ shape=oval label="(37) [Function Exit]" ];
                           |  38 [ shape=oval label="(38) [Empty Node]" ];
                           |  8 -> 6 [ label="0.0" ];
                           |  7 -> 8 [ label="0.0" ];
                           |  3 -> 4 [ label="1.0" ];
                           |  3 -> 5 [ label="-1.0" ];
                           |  4 -> 7 [ label="0.0" ];
                           |  5 -> 6 [ label="0.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  6 -> 1 [ label="0.0" ];
                           |  10 -> 11 [ label="1.0" ];
                           |  10 -> 12 [ label="-1.0" ];
                           |  11 -> 13 [ label="0.0" ];
                           |  12 -> 14 [ label="0.0" ];
                           |  15 -> 9 [ label="0.0" ];
                           |  13 -> 15 [ label="0.0" ];
                           |  14 -> 15 [ label="0.0" ];
                           |  17 -> 18 [ label="1.0" ];
                           |  17 -> 19 [ label="-1.0" ];
                           |  18 -> 20 [ label="0.0" ];
                           |  19 -> 21 [ label="0.0" ];
                           |  20 -> 16 [ label="0.0" ];
                           |  21 -> 16 [ label="0.0" ];
                           |  23 -> 22 [ label="0.0" ];
                           |  29 -> 24 [ label="0.0" ];
                           |  30 -> 24 [ label="0.0" ];
                           |  26 -> 27 [ label="1.0" ];
                           |  26 -> 28 [ label="-1.0" ];
                           |  27 -> 29 [ label="0.0" ];
                           |  28 -> 30 [ label="0.0" ];
                           |  25 -> 26 [ label="0.0" ];
                           |  34 -> 31 [ label="0.0" ];
                           |  32 -> 33 [ label="0.0" ];
                           |  33 -> 34 [ label="0.0" ];
                           |  36 -> 35 [ label="0.0" ];
                           |  38 -> 37 [ label="0.0" ];
                           |}""".stripMargin

    val test02 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = Loop(LessThan(i, Number(10)), Block(List(Continue(), Assignment(i, Addition(i, Number(1))))))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)), Set())
      BrboProgram("test02", main, Nil)
    }
    val test02Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=oval label="(3) [Branch Head]" ];
                           |  4 [ shape=diamond label="(4) (i < 10)" ];
                           |  5 [ shape=diamond label="(5) !((i < 10))" ];
                           |  6 [ shape=oval label="(6) [Loop Exit]" ];
                           |  7 [ shape=rectangle label="(7) continue;" ];
                           |  8 [ shape=rectangle label="(8) i = i + 1;" ];
                           |  7 -> 3 [ label="0.0" ];
                           |  3 -> 4 [ label="1.0" ];
                           |  3 -> 5 [ label="-1.0" ];
                           |  4 -> 7 [ label="0.0" ];
                           |  5 -> 6 [ label="0.0" ];
                           |  8 -> 4 [ label="0.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  6 -> 1 [ label="0.0" ];
                           |}""".stripMargin

    val test03 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, FunctionCallExpr("ndInt2", List(Number(0), Number(1)), INT))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration)), Set())
      BrboProgram("test03", main, Nil)
    }
    val test03Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = ndInt2(0, 1);" ];
                           |  2 -> 1 [ label="0.0" ];
                           |}""".stripMargin

    val test04 = {
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
      BrboProgram("test04", main, Nil)
    }
    val test04Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=oval label="(3) [Branch Head]" ];
                           |  4 [ shape=diamond label="(4) (i < 5)" ];
                           |  5 [ shape=diamond label="(5) !((i < 5))" ];
                           |  6 [ shape=rectangle label="(6) i = 1;" ];
                           |  7 [ shape=rectangle label="(7) i = 2;" ];
                           |  8 [ shape=rectangle label="(8) i = 3;" ];
                           |  3 -> 4 [ label="1.0" ];
                           |  3 -> 5 [ label="-1.0" ];
                           |  4 -> 6 [ label="0.0" ];
                           |  5 -> 7 [ label="0.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  6 -> 8 [ label="0.0" ];
                           |  7 -> 8 [ label="0.0" ];
                           |  8 -> 1 [ label="0.0" ];
                           |}""".stripMargin

    val test05 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = {
        val loop = {
          val ite = ITE(LessThan(i, Number(10)), Break(), Continue())
          Loop(LessThan(i, Number(7)), ite)
        }
        val assignment = Assignment(i, Number(12))
        Loop(LessThan(i, Number(5)), Block(List(loop, assignment)))
      }
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)), Set())
      BrboProgram("test05", main, Nil)
    }
    val test05Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=oval label="(3) [Branch Head]" ];
                           |  4 [ shape=diamond label="(4) (i < 5)" ];
                           |  5 [ shape=diamond label="(5) !((i < 5))" ];
                           |  6 [ shape=oval label="(6) [Loop Exit]" ];
                           |  7 [ shape=oval label="(7) [Branch Head]" ];
                           |  8 [ shape=diamond label="(8) (i < 7)" ];
                           |  9 [ shape=diamond label="(9) !((i < 7))" ];
                           |  10 [ shape=oval label="(10) [Loop Exit]" ];
                           |  11 [ shape=oval label="(11) [Branch Head]" ];
                           |  12 [ shape=diamond label="(12) (i < 10)" ];
                           |  13 [ shape=diamond label="(13) !((i < 10))" ];
                           |  14 [ shape=rectangle label="(14) break;" ];
                           |  15 [ shape=rectangle label="(15) continue;" ];
                           |  16 [ shape=rectangle label="(16) i = 12;" ];
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
                           |  16 -> 4 [ label="0.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  6 -> 1 [ label="0.0" ];
                           |}""".stripMargin

    val test06 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val reset: Reset = Reset(1, LessThanOrEqualTo(i, Number(0)))
      val use: Use = Use(Some(1), i, GreaterThan(i, Number(5)))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, reset, use)), Set())
      BrboProgram("test06", main, Nil, Nil)
    }
    val test06Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=rectangle label="(3) if (i <= 0) reset R1" ];
                           |  4 [ shape=rectangle label="(4) if (i > 5) use R1 i" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  3 -> 4 [ label="0.0" ];
                           |  4 -> 1 [ label="0.0" ];
                           |}""".stripMargin

    List[TestCase](
      TestCase("Test `break`", test01, test01Expected),
      TestCase("Test `continue`", test02, test02Expected),
      TestCase("Test `function calls (There must be no edges for function calls)`", test03, test03Expected),
      TestCase("Test `ite`", test04, test04Expected),
      TestCase("Test nested loop", test05, test05Expected),
      TestCase("Test ghost commands", test06, test06Expected),
    )
  }
}