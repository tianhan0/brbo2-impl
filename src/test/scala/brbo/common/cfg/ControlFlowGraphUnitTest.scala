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
                           |  3 [ shape=diamond label="(3) (i < 10)" ];
                           |  4 [ shape=oval label="(4) [Loop Exit]" ];
                           |  5 [ shape=rectangle label="(5) i = i + 1;" ];
                           |  6 [ shape=rectangle label="(6) break;" ];
                           |  7 [ shape=oval label="(7) [Function Exit]" ];
                           |  8 [ shape=diamond label="(8) !(cond)" ];
                           |  9 [ shape=rectangle label="(9) ERROR: __VERIFIER_error();" ];
                           |  10 [ shape=rectangle label="(10) ;" ];
                           |  11 [ shape=rectangle label="(11) return;" ];
                           |  12 [ shape=oval label="(12) [Function Exit]" ];
                           |  13 [ shape=diamond label="(13) !(cond)" ];
                           |  14 [ shape=rectangle label="(14) abort();" ];
                           |  15 [ shape=rectangle label="(15) ;" ];
                           |  16 [ shape=oval label="(16) [Function Exit]" ];
                           |  17 [ shape=rectangle label="(17) return __VERIFIER_nondet_int();" ];
                           |  18 [ shape=oval label="(18) [Function Exit]" ];
                           |  19 [ shape=rectangle label="(19) int x = ndInt();" ];
                           |  20 [ shape=diamond label="(20) (x > 0)" ];
                           |  21 [ shape=rectangle label="(21) return true;" ];
                           |  22 [ shape=rectangle label="(22) return false;" ];
                           |  23 [ shape=oval label="(23) [Function Exit]" ];
                           |  24 [ shape=rectangle label="(24) int x = ndInt();" ];
                           |  25 [ shape=rectangle label="(25) assume((lower <= x) && (x <= upper));" ];
                           |  26 [ shape=rectangle label="(26) return x;" ];
                           |  27 [ shape=oval label="(27) [Function Exit]" ];
                           |  28 [ shape=rectangle label="(28) assume((lower <= x) && (x <= upper));" ];
                           |  29 [ shape=oval label="(29) [Function Exit]" ];
                           |  30 [ shape=oval label="(30) [Empty node]" ];
                           |  6 -> 4 [ label="0.0" ];
                           |  5 -> 6 [ label="0.0" ];
                           |  3 -> 4 [ label="-1.0" ];
                           |  3 -> 5 [ label="1.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  4 -> 1 [ label="0.0" ];
                           |  8 -> 9 [ label="1.0" ];
                           |  8 -> 10 [ label="-1.0" ];
                           |  11 -> 7 [ label="0.0" ];
                           |  9 -> 11 [ label="0.0" ];
                           |  10 -> 11 [ label="0.0" ];
                           |  13 -> 14 [ label="1.0" ];
                           |  13 -> 15 [ label="-1.0" ];
                           |  14 -> 12 [ label="0.0" ];
                           |  15 -> 12 [ label="0.0" ];
                           |  17 -> 16 [ label="0.0" ];
                           |  21 -> 18 [ label="0.0" ];
                           |  22 -> 18 [ label="0.0" ];
                           |  20 -> 21 [ label="1.0" ];
                           |  20 -> 22 [ label="-1.0" ];
                           |  19 -> 20 [ label="0.0" ];
                           |  26 -> 23 [ label="0.0" ];
                           |  24 -> 25 [ label="0.0" ];
                           |  25 -> 26 [ label="0.0" ];
                           |  28 -> 27 [ label="0.0" ];
                           |  30 -> 29 [ label="0.0" ];
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
                           |  3 [ shape=diamond label="(3) (i < 10)" ];
                           |  4 [ shape=oval label="(4) [Loop Exit]" ];
                           |  5 [ shape=rectangle label="(5) continue;" ];
                           |  6 [ shape=rectangle label="(6) i = i + 1;" ];
                           |  5 -> 3 [ label="0.0" ];
                           |  3 -> 4 [ label="-1.0" ];
                           |  3 -> 5 [ label="1.0" ];
                           |  6 -> 3 [ label="0.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  4 -> 1 [ label="0.0" ];
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
                           |  3 [ shape=diamond label="(3) (i < 5)" ];
                           |  4 [ shape=rectangle label="(4) i = 1;" ];
                           |  5 [ shape=rectangle label="(5) i = 2;" ];
                           |  6 [ shape=rectangle label="(6) i = 3;" ];
                           |  3 -> 4 [ label="1.0" ];
                           |  3 -> 5 [ label="-1.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  4 -> 6 [ label="0.0" ];
                           |  5 -> 6 [ label="0.0" ];
                           |  6 -> 1 [ label="0.0" ];
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
                           |  3 [ shape=diamond label="(3) (i < 5)" ];
                           |  4 [ shape=oval label="(4) [Loop Exit]" ];
                           |  5 [ shape=diamond label="(5) (i < 7)" ];
                           |  6 [ shape=oval label="(6) [Loop Exit]" ];
                           |  7 [ shape=diamond label="(7) (i < 10)" ];
                           |  8 [ shape=rectangle label="(8) break;" ];
                           |  9 [ shape=rectangle label="(9) continue;" ];
                           |  10 [ shape=rectangle label="(10) i = 12;" ];
                           |  8 -> 6 [ label="0.0" ];
                           |  9 -> 5 [ label="0.0" ];
                           |  7 -> 8 [ label="1.0" ];
                           |  7 -> 9 [ label="-1.0" ];
                           |  5 -> 6 [ label="-1.0" ];
                           |  5 -> 7 [ label="1.0" ];
                           |  6 -> 10 [ label="0.0" ];
                           |  3 -> 4 [ label="-1.0" ];
                           |  3 -> 5 [ label="1.0" ];
                           |  10 -> 3 [ label="0.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  4 -> 1 [ label="0.0" ];
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