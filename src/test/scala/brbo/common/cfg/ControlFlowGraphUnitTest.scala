package brbo.common.cfg

import brbo.{StringCompare, TestCase}
import brbo.common.TypeUtils.BrboType.{INT, VOID}
import brbo.common.ast._
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.flatspec.AnyFlatSpec

class ControlFlowGraphUnitTest extends AnyFlatSpec {
  val logger: Logger = LogManager.getLogger(classOf[ControlFlowGraphUnitTest])

  ControlFlowGraphUnitTest.testCases.foreach({
    testCase =>
      val controlFlowGraph = ControlFlowGraph.toControlFlowGraph(testCase.input.asInstanceOf[BrboProgram])
      controlFlowGraph.printPDF()
      val dotRepresentation = controlFlowGraph.exportToDOT
      assert(StringCompare.ignoreWhitespaces(testCase.expectedOutput, dotRepresentation, s"${testCase.name} failed!"))
  })
}

object ControlFlowGraphUnitTest {
  val testCases: List[TestCase] = {
    val test01 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = Loop(LessThan(i, Number(10)), Block(List(Assignment(i, Addition(i, Number(1))), Break())))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)))
      BrboProgram("test01", main, PreDefinedBrboFunctions.allFunctions)
    }
    val test01Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=diamond label="(3) (i < 10)" ];
                           |  4 [ shape=oval label="(4) [Loop Exit]" ];
                           |  5 [ shape=rectangle label="(5) i = (i + 1);" ];
                           |  6 [ shape=rectangle label="(6) break;" ];
                           |  7 [ shape=oval label="(7) [Function Exit]" ];
                           |  8 [ shape=diamond label="(8) !(cond)" ];
                           |  9 [ shape=rectangle label="(9) ERROR: __VERIFIER_error();" ];
                           |  10 [ shape=rectangle label="(10) ;" ];
                           |  11 [ shape=rectangle label="(11) return;" ];
                           |  12 [ shape=oval label="(12) [Function Exit]" ];
                           |  13 [ shape=diamond label="(13) !(cond)" ];
                           |  14 [ shape=rectangle label="(14) ERROR: __VERIFIER_error();" ];
                           |  15 [ shape=rectangle label="(15) ;" ];
                           |  16 [ shape=rectangle label="(16) return;" ];
                           |  17 [ shape=oval label="(17) [Function Exit]" ];
                           |  18 [ shape=rectangle label="(18) return __VERIFIER_nondet_int();" ];
                           |  19 [ shape=oval label="(19) [Function Exit]" ];
                           |  20 [ shape=rectangle label="(20) int x = ndInt();" ];
                           |  21 [ shape=rectangle label="(21) assume(((x == 0) || (x == 1)));" ];
                           |  22 [ shape=rectangle label="(22) return x;" ];
                           |  23 [ shape=oval label="(23) [Function Exit]" ];
                           |  24 [ shape=rectangle label="(24) int x = ndInt();" ];
                           |  25 [ shape=rectangle label="(25) assume(((lower <= x) && (x <= upper)));" ];
                           |  26 [ shape=rectangle label="(26) return x;" ];
                           |  6 -> 4;
                           |  5 -> 6;
                           |  3 -> 4;
                           |  3 -> 5;
                           |  2 -> 3;
                           |  4 -> 1;
                           |  8 -> 9;
                           |  8 -> 10;
                           |  11 -> 7;
                           |  9 -> 11;
                           |  10 -> 11;
                           |  13 -> 14;
                           |  13 -> 15;
                           |  16 -> 12;
                           |  14 -> 16;
                           |  15 -> 16;
                           |  18 -> 17;
                           |  22 -> 19;
                           |  20 -> 21;
                           |  21 -> 22;
                           |  26 -> 23;
                           |  24 -> 25;
                           |  25 -> 26;
                           |}""".stripMargin

    val test02 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, Number(0))
      val loop = Loop(LessThan(i, Number(10)), Block(List(Continue(), Assignment(i, Addition(i, Number(1))))))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration, loop)))
      BrboProgram("test02", main, PreDefinedBrboFunctions.allFunctions)
    }
    val test02Expected = """strict digraph G {
                           |  1 [ shape=oval label="(1) [Function Exit]" ];
                           |  2 [ shape=rectangle label="(2) int i = 0;" ];
                           |  3 [ shape=diamond label="(3) (i < 10)" ];
                           |  4 [ shape=oval label="(4) [Loop Exit]" ];
                           |  5 [ shape=rectangle label="(5) continue;" ];
                           |  6 [ shape=rectangle label="(6) i = (i + 1);" ];
                           |  7 [ shape=oval label="(7) [Function Exit]" ];
                           |  8 [ shape=diamond label="(8) !(cond)" ];
                           |  9 [ shape=rectangle label="(9) ERROR: __VERIFIER_error();" ];
                           |  10 [ shape=rectangle label="(10) ;" ];
                           |  11 [ shape=rectangle label="(11) return;" ];
                           |  12 [ shape=oval label="(12) [Function Exit]" ];
                           |  13 [ shape=diamond label="(13) !(cond)" ];
                           |  14 [ shape=rectangle label="(14) ERROR: __VERIFIER_error();" ];
                           |  15 [ shape=rectangle label="(15) ;" ];
                           |  16 [ shape=rectangle label="(16) return;" ];
                           |  17 [ shape=oval label="(17) [Function Exit]" ];
                           |  18 [ shape=rectangle label="(18) return __VERIFIER_nondet_int();" ];
                           |  19 [ shape=oval label="(19) [Function Exit]" ];
                           |  20 [ shape=rectangle label="(20) int x = ndInt();" ];
                           |  21 [ shape=rectangle label="(21) assume(((x == 0) || (x == 1)));" ];
                           |  22 [ shape=rectangle label="(22) return x;" ];
                           |  23 [ shape=oval label="(23) [Function Exit]" ];
                           |  24 [ shape=rectangle label="(24) int x = ndInt();" ];
                           |  25 [ shape=rectangle label="(25) assume(((lower <= x) && (x <= upper)));" ];
                           |  26 [ shape=rectangle label="(26) return x;" ];
                           |  5 -> 3;
                           |  3 -> 4;
                           |  3 -> 5;
                           |  6 -> 3;
                           |  2 -> 3;
                           |  4 -> 1;
                           |  8 -> 9;
                           |  8 -> 10;
                           |  11 -> 7;
                           |  9 -> 11;
                           |  10 -> 11;
                           |  13 -> 14;
                           |  13 -> 15;
                           |  16 -> 12;
                           |  14 -> 16;
                           |  15 -> 16;
                           |  18 -> 17;
                           |  22 -> 19;
                           |  20 -> 21;
                           |  21 -> 22;
                           |  26 -> 23;
                           |  24 -> 25;
                           |  25 -> 26;
                           |}""".stripMargin

    val test03 = {
      val i = Identifier("i", INT)
      val variableDeclaration = VariableDeclaration(i, FunctionCallExpr("ndInt2", List(Number(0), Number(1)), INT))
      val main = BrboFunction("main", VOID, Nil, Block(List(variableDeclaration)))
      BrboProgram("test03", main, PreDefinedBrboFunctions.allFunctions)
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
                           |  10 [ shape=rectangle label="(10) ERROR: __VERIFIER_error();" ];
                           |  11 [ shape=rectangle label="(11) ;" ];
                           |  12 [ shape=rectangle label="(12) return;" ];
                           |  13 [ shape=oval label="(13) [Function Exit]" ];
                           |  14 [ shape=rectangle label="(14) return __VERIFIER_nondet_int();" ];
                           |  15 [ shape=oval label="(15) [Function Exit]" ];
                           |  16 [ shape=rectangle label="(16) int x = ndInt();" ];
                           |  17 [ shape=rectangle label="(17) assume(((x == 0) || (x == 1)));" ];
                           |  18 [ shape=rectangle label="(18) return x;" ];
                           |  19 [ shape=oval label="(19) [Function Exit]" ];
                           |  20 [ shape=rectangle label="(20) int x = ndInt();" ];
                           |  21 [ shape=rectangle label="(21) assume(((lower <= x) && (x <= upper)));" ];
                           |  22 [ shape=rectangle label="(22) return x;" ];
                           |  2 -> 1;
                           |  4 -> 5;
                           |  4 -> 6;
                           |  7 -> 3;
                           |  5 -> 7;
                           |  6 -> 7;
                           |  9 -> 10;
                           |  9 -> 11;
                           |  12 -> 8;
                           |  10 -> 12;
                           |  11 -> 12;
                           |  14 -> 13;
                           |  18 -> 15;
                           |  16 -> 17;
                           |  17 -> 18;
                           |  22 -> 19;
                           |  20 -> 21;
                           |  21 -> 22;
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
      BrboProgram("test04", main, PreDefinedBrboFunctions.allFunctions)
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
                           |  14 [ shape=rectangle label="(14) ERROR: __VERIFIER_error();" ];
                           |  15 [ shape=rectangle label="(15) ;" ];
                           |  16 [ shape=rectangle label="(16) return;" ];
                           |  17 [ shape=oval label="(17) [Function Exit]" ];
                           |  18 [ shape=rectangle label="(18) return __VERIFIER_nondet_int();" ];
                           |  19 [ shape=oval label="(19) [Function Exit]" ];
                           |  20 [ shape=rectangle label="(20) int x = ndInt();" ];
                           |  21 [ shape=rectangle label="(21) assume(((x == 0) || (x == 1)));" ];
                           |  22 [ shape=rectangle label="(22) return x;" ];
                           |  23 [ shape=oval label="(23) [Function Exit]" ];
                           |  24 [ shape=rectangle label="(24) int x = ndInt();" ];
                           |  25 [ shape=rectangle label="(25) assume(((lower <= x) && (x <= upper)));" ];
                           |  26 [ shape=rectangle label="(26) return x;" ];
                           |  3 -> 4;
                           |  3 -> 5;
                           |  2 -> 3;
                           |  4 -> 6;
                           |  5 -> 6;
                           |  6 -> 1;
                           |  8 -> 9;
                           |  8 -> 10;
                           |  11 -> 7;
                           |  9 -> 11;
                           |  10 -> 11;
                           |  13 -> 14;
                           |  13 -> 15;
                           |  16 -> 12;
                           |  14 -> 16;
                           |  15 -> 16;
                           |  18 -> 17;
                           |  22 -> 19;
                           |  20 -> 21;
                           |  21 -> 22;
                           |  26 -> 23;
                           |  24 -> 25;
                           |  25 -> 26;
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
      BrboProgram("test05", main, PreDefinedBrboFunctions.allFunctions)
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
                           |  18 [ shape=rectangle label="(18) ERROR: __VERIFIER_error();" ];
                           |  19 [ shape=rectangle label="(19) ;" ];
                           |  20 [ shape=rectangle label="(20) return;" ];
                           |  21 [ shape=oval label="(21) [Function Exit]" ];
                           |  22 [ shape=rectangle label="(22) return __VERIFIER_nondet_int();" ];
                           |  23 [ shape=oval label="(23) [Function Exit]" ];
                           |  24 [ shape=rectangle label="(24) int x = ndInt();" ];
                           |  25 [ shape=rectangle label="(25) assume(((x == 0) || (x == 1)));" ];
                           |  26 [ shape=rectangle label="(26) return x;" ];
                           |  27 [ shape=oval label="(27) [Function Exit]" ];
                           |  28 [ shape=rectangle label="(28) int x = ndInt();" ];
                           |  29 [ shape=rectangle label="(29) assume(((lower <= x) && (x <= upper)));" ];
                           |  30 [ shape=rectangle label="(30) return x;" ];
                           |  8 -> 6;
                           |  9 -> 5;
                           |  7 -> 8;
                           |  7 -> 9;
                           |  5 -> 6;
                           |  5 -> 7;
                           |  6 -> 10;
                           |  3 -> 4;
                           |  3 -> 5;
                           |  10 -> 3;
                           |  2 -> 3;
                           |  4 -> 1;
                           |  12 -> 13;
                           |  12 -> 14;
                           |  15 -> 11;
                           |  13 -> 15;
                           |  14 -> 15;
                           |  17 -> 18;
                           |  17 -> 19;
                           |  20 -> 16;
                           |  18 -> 20;
                           |  19 -> 20;
                           |  22 -> 21;
                           |  26 -> 23;
                           |  24 -> 25;
                           |  25 -> 26;
                           |  30 -> 27;
                           |  28 -> 29;
                           |  29 -> 30;
                           |}""".stripMargin

    List[TestCase](
      TestCase("Test `break`", test01, test01Expected),
      TestCase("Test `continue`", test02, test02Expected),
      TestCase("Test function calls: There must be no edges for function calls!", test03, test03Expected),
      TestCase("Test `ite`", test04, test04Expected),
      TestCase("Test nested loop", test05, test05Expected),
    )
  }
}