package brbo.common.cfg

import brbo.TestCase
import brbo.common.StringCompare
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
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
        assert(StringCompare.ignoreWhitespaces(dotRepresentation, testCase.expectedOutput, s"${testCase.name} failed!"))
    })
  }
}

object ControlFlowGraphUnitTest {
  val testCases: List[TestCase] = {
    val test01 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = Loop(LessThan(i, Number(10)), Block(List(Assignment(i, Addition(i, Number(1))), Break())))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)))
      BrboProgram("test01", main, Set[Int](), None, None, PreDefinedFunctions.allFunctionsList)
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
                           |  20 [ shape=rectangle label="(20) assume((x == 0) || (x == 1));" ];
                           |  21 [ shape=rectangle label="(21) return x;" ];
                           |  22 [ shape=oval label="(22) [Function Exit]" ];
                           |  23 [ shape=rectangle label="(23) int x = ndInt();" ];
                           |  24 [ shape=rectangle label="(24) assume((lower <= x) && (x <= upper));" ];
                           |  25 [ shape=rectangle label="(25) return x;" ];
                           |  26 [ shape=oval label="(26) [Function Exit]" ];
                           |  27 [ shape=oval label="(27) [Empty node]" ];
                           |  28 [ shape=oval label="(28) [Function Exit]" ];
                           |  29 [ shape=oval label="(29) [Empty node]" ];
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
                           |  19 -> 20 [ label="0.0" ];
                           |  20 -> 21 [ label="0.0" ];
                           |  25 -> 22 [ label="0.0" ];
                           |  23 -> 24 [ label="0.0" ];
                           |  24 -> 25 [ label="0.0" ];
                           |  27 -> 26 [ label="0.0" ];
                           |  29 -> 28 [ label="0.0" ];
                           |}""".stripMargin

    val test02 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = Loop(LessThan(i, Number(10)), Block(List(Continue(), Assignment(i, Addition(i, Number(1))))))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)))
      BrboProgram("test02", main, Set[Int](), None, None, PreDefinedFunctions.allFunctionsList)
    }
    val test02Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=diamond label="(3) (i < 10)" ];
                           |  4 [ shape=oval label="(4) [Loop Exit]" ];
                           |  5 [ shape=rectangle label="(5) continue;" ];
                           |  6 [ shape=rectangle label="(6) i = i + 1;" ];
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
                           |  20 [ shape=rectangle label="(20) assume((x == 0) || (x == 1));" ];
                           |  21 [ shape=rectangle label="(21) return x;" ];
                           |  22 [ shape=oval label="(22) [Function Exit]" ];
                           |  23 [ shape=rectangle label="(23) int x = ndInt();" ];
                           |  24 [ shape=rectangle label="(24) assume((lower <= x) && (x <= upper));" ];
                           |  25 [ shape=rectangle label="(25) return x;" ];
                           |  26 [ shape=oval label="(26) [Function Exit]" ];
                           |  27 [ shape=oval label="(27) [Empty node]" ];
                           |  28 [ shape=oval label="(28) [Function Exit]" ];
                           |  29 [ shape=oval label="(29) [Empty node]" ];
                           |  5 -> 3 [ label="0.0" ];
                           |  3 -> 4 [ label="-1.0" ];
                           |  3 -> 5 [ label="1.0" ];
                           |  6 -> 3 [ label="0.0" ];
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
                           |  19 -> 20 [ label="0.0" ];
                           |  20 -> 21 [ label="0.0" ];
                           |  25 -> 22 [ label="0.0" ];
                           |  23 -> 24 [ label="0.0" ];
                           |  24 -> 25 [ label="0.0" ];
                           |  27 -> 26 [ label="0.0" ];
                           |  29 -> 28 [ label="0.0" ];
                           |}""".stripMargin

    val test03 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, FunctionCallExpr("ndInt2", List(Number(0), Number(1)), INT))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration)))
      BrboProgram("test03", main, Set[Int](), None, None, PreDefinedFunctions.allFunctionsList)
    }
    val test03Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = ndInt2(0, 1);" ];
                           |  3 [ shape=oval label="(3) [Function Exit]" ];
                           |  4 [ shape=diamond label="(4) !(cond)" ];
                           |  5 [ shape=rectangle label="(5) ERROR: __VERIFIER_error();" ];
                           |  6 [ shape=rectangle label="(6) ;" ];
                           |  7 [ shape=rectangle label="(7) return;" ];
                           |  8 [ shape=oval label="(8) [Function Exit]" ];
                           |  9 [ shape=diamond label="(9) !(cond)" ];
                           |  10 [ shape=rectangle label="(10) abort();" ];
                           |  11 [ shape=rectangle label="(11) ;" ];
                           |  12 [ shape=oval label="(12) [Function Exit]" ];
                           |  13 [ shape=rectangle label="(13) return __VERIFIER_nondet_int();" ];
                           |  14 [ shape=oval label="(14) [Function Exit]" ];
                           |  15 [ shape=rectangle label="(15) int x = ndInt();" ];
                           |  16 [ shape=rectangle label="(16) assume((x == 0) || (x == 1));" ];
                           |  17 [ shape=rectangle label="(17) return x;" ];
                           |  18 [ shape=oval label="(18) [Function Exit]" ];
                           |  19 [ shape=rectangle label="(19) int x = ndInt();" ];
                           |  20 [ shape=rectangle label="(20) assume((lower <= x) && (x <= upper));" ];
                           |  21 [ shape=rectangle label="(21) return x;" ];
                           |  22 [ shape=oval label="(22) [Function Exit]" ];
                           |  23 [ shape=oval label="(23) [Empty node]" ];
                           |  24 [ shape=oval label="(24) [Function Exit]" ];
                           |  25 [ shape=oval label="(25) [Empty node]" ];
                           |  2 -> 1 [ label="0.0" ];
                           |  4 -> 5 [ label="1.0" ];
                           |  4 -> 6 [ label="-1.0" ];
                           |  7 -> 3 [ label="0.0" ];
                           |  5 -> 7 [ label="0.0" ];
                           |  6 -> 7 [ label="0.0" ];
                           |  9 -> 10 [ label="1.0" ];
                           |  9 -> 11 [ label="-1.0" ];
                           |  10 -> 8 [ label="0.0" ];
                           |  11 -> 8 [ label="0.0" ];
                           |  13 -> 12 [ label="0.0" ];
                           |  17 -> 14 [ label="0.0" ];
                           |  15 -> 16 [ label="0.0" ];
                           |  16 -> 17 [ label="0.0" ];
                           |  21 -> 18 [ label="0.0" ];
                           |  19 -> 20 [ label="0.0" ];
                           |  20 -> 21 [ label="0.0" ];
                           |  23 -> 22 [ label="0.0" ];
                           |  25 -> 24 [ label="0.0" ];
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
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, ite, assignment)))
      BrboProgram("test04", main, Set[Int](), None, None, PreDefinedFunctions.allFunctionsList)
    }
    val test04Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=diamond label="(3) (i < 5)" ];
                           |  4 [ shape=rectangle label="(4) i = 1;" ];
                           |  5 [ shape=rectangle label="(5) i = 2;" ];
                           |  6 [ shape=rectangle label="(6) i = 3;" ];
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
                           |  20 [ shape=rectangle label="(20) assume((x == 0) || (x == 1));" ];
                           |  21 [ shape=rectangle label="(21) return x;" ];
                           |  22 [ shape=oval label="(22) [Function Exit]" ];
                           |  23 [ shape=rectangle label="(23) int x = ndInt();" ];
                           |  24 [ shape=rectangle label="(24) assume((lower <= x) && (x <= upper));" ];
                           |  25 [ shape=rectangle label="(25) return x;" ];
                           |  26 [ shape=oval label="(26) [Function Exit]" ];
                           |  27 [ shape=oval label="(27) [Empty node]" ];
                           |  28 [ shape=oval label="(28) [Function Exit]" ];
                           |  29 [ shape=oval label="(29) [Empty node]" ];
                           |  3 -> 4 [ label="1.0" ];
                           |  3 -> 5 [ label="-1.0" ];
                           |  2 -> 3 [ label="0.0" ];
                           |  4 -> 6 [ label="0.0" ];
                           |  5 -> 6 [ label="0.0" ];
                           |  6 -> 1 [ label="0.0" ];
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
                           |  19 -> 20 [ label="0.0" ];
                           |  20 -> 21 [ label="0.0" ];
                           |  25 -> 22 [ label="0.0" ];
                           |  23 -> 24 [ label="0.0" ];
                           |  24 -> 25 [ label="0.0" ];
                           |  27 -> 26 [ label="0.0" ];
                           |  29 -> 28 [ label="0.0" ];
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
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)))
      BrboProgram("test05", main, Set[Int](), None, None, PreDefinedFunctions.allFunctionsList)
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
                           |  11 [ shape=oval label="(11) [Function Exit]" ];
                           |  12 [ shape=diamond label="(12) !(cond)" ];
                           |  13 [ shape=rectangle label="(13) ERROR: __VERIFIER_error();" ];
                           |  14 [ shape=rectangle label="(14) ;" ];
                           |  15 [ shape=rectangle label="(15) return;" ];
                           |  16 [ shape=oval label="(16) [Function Exit]" ];
                           |  17 [ shape=diamond label="(17) !(cond)" ];
                           |  18 [ shape=rectangle label="(18) abort();" ];
                           |  19 [ shape=rectangle label="(19) ;" ];
                           |  20 [ shape=oval label="(20) [Function Exit]" ];
                           |  21 [ shape=rectangle label="(21) return __VERIFIER_nondet_int();" ];
                           |  22 [ shape=oval label="(22) [Function Exit]" ];
                           |  23 [ shape=rectangle label="(23) int x = ndInt();" ];
                           |  24 [ shape=rectangle label="(24) assume((x == 0) || (x == 1));" ];
                           |  25 [ shape=rectangle label="(25) return x;" ];
                           |  26 [ shape=oval label="(26) [Function Exit]" ];
                           |  27 [ shape=rectangle label="(27) int x = ndInt();" ];
                           |  28 [ shape=rectangle label="(28) assume((lower <= x) && (x <= upper));" ];
                           |  29 [ shape=rectangle label="(29) return x;" ];
                           |  30 [ shape=oval label="(30) [Function Exit]" ];
                           |  31 [ shape=oval label="(31) [Empty node]" ];
                           |  32 [ shape=oval label="(32) [Function Exit]" ];
                           |  33 [ shape=oval label="(33) [Empty node]" ];
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
                           |  12 -> 13 [ label="1.0" ];
                           |  12 -> 14 [ label="-1.0" ];
                           |  15 -> 11 [ label="0.0" ];
                           |  13 -> 15 [ label="0.0" ];
                           |  14 -> 15 [ label="0.0" ];
                           |  17 -> 18 [ label="1.0" ];
                           |  17 -> 19 [ label="-1.0" ];
                           |  18 -> 16 [ label="0.0" ];
                           |  19 -> 16 [ label="0.0" ];
                           |  21 -> 20 [ label="0.0" ];
                           |  25 -> 22 [ label="0.0" ];
                           |  23 -> 24 [ label="0.0" ];
                           |  24 -> 25 [ label="0.0" ];
                           |  29 -> 26 [ label="0.0" ];
                           |  27 -> 28 [ label="0.0" ];
                           |  28 -> 29 [ label="0.0" ];
                           |  31 -> 30 [ label="0.0" ];
                           |  33 -> 32 [ label="0.0" ];
                           |}""".stripMargin

    List[TestCase](
      TestCase("Test `break`", test01, test01Expected),
      TestCase("Test `continue`", test02, test02Expected),
      TestCase("Test `function calls (There must be no edges for function calls)`", test03, test03Expected),
      TestCase("Test `ite`", test04, test04Expected),
      TestCase("Test nested loop", test05, test05Expected),
    )
  }
}