package brbo.common.ast

import brbo.TestCase
import brbo.common.BrboType
import brbo.common.BrboType.INT
import brbo.common.string.StringCompare
import brbo.frontend.BasicProcessor
import org.scalatest.flatspec.AnyFlatSpec

class BrboAstUnitTest extends AnyFlatSpec {
  "Any two instances of AST nodes" should "be different" in {
    import BrboAstUnitTest._
    assert(createContinue != createContinue)
    assert(createBreak != createBreak)
    assert(createSkip != createSkip)
    assert(createReturn != createReturn)
    assert(createFunctionCall != createFunctionCall)
    assert(createAssignment != createAssignment)
    assert(createVariableDeclaration != createVariableDeclaration)
    // assert(createAssert != createAssert)
    // assert(createAssume != createAssume)
    assert(createLabeledCommand != createLabeledCommand)
    assert(createITE != createITE)
    assert(createLoop != createLoop)
    assert(createBlock != createBlock)
    assert(FunctionExit() != FunctionExit())
    assert(LoopExit() != LoopExit())
    assert(createUse != createUse)
    assert(createUse2 != createUse2)
    assert(createReset != createReset)
  }

  "Pretty-printing BrboAst to C statements" should "be correct" in {
    BrboAstUnitTest.prettyPrintToCUnitTest.foreach({
      testCase =>
        StringCompare.compareLiteral(
          testCase.input.asInstanceOf[BrboAst].printToC(2),
          testCase.expectedOutput,
          s"${testCase.name} failed!"
        )
    })
  }

  "Parsing pre-defined functions" should "be correct" in {
    BrboAstUnitTest.parsePredefinedFunctions.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        StringCompare.ignoreWhitespaces(targetProgram.program.mainFunction.printToC(0), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object BrboAstUnitTest {
  val prettyPrintToCUnitTest: List[TestCase] =
    List[TestCase](
      TestCase("Continue", createContinue, "  continue;"),
      TestCase("Break", createBreak, "  break;"),
      TestCase("Skip", createSkip, "  ;"),
      TestCase("Return", createReturn, "  return x;"),
      TestCase("FunctionCall", createFunctionCall, "  f(a, b);"),
      TestCase("Assignment", createAssignment, "  x = 0;"),
      TestCase("VariableDeclaration", createVariableDeclaration, "  int x = 1;"),
      // TestCase("Assert", createAssert, "  assert(false);"),
      // TestCase("Assume", createAssume, "  assume(false);"),
      TestCase("LabeledCommand", createLabeledCommand, "  label: return;"),
      TestCase("ITE", createITE, "  if (true)\n    x = 0;\n  else\n    x = 1;"),
      TestCase("Loop", createLoop, "  while (0 < x)\n  {\n    x = 0;\n    x = 1;\n  }"),
      TestCase("Block", createBlock, "  {\n    x = 0;\n    x = 1;\n  }"),
      TestCase("Use", createUse, "  if (true) R5 = R5 + 1;"),
      TestCase("Use 2", createUse2, "  if (true) R = R + 2;"),
      TestCase("Reset", createReset,
        """  if (true) {
          |    if (S5 < R5)
          |      S5 = R5;
          |    else
          |      ;
          |    R5 = 0;
          |    C5 = C5 + 1;
          |  }""".stripMargin)
    )

  def createContinue: Continue = Continue()

  def createBreak: Break = Break()

  def createSkip: Skip = Skip()

  def createReturn: Return = Return(Some(Identifier("x", INT)))

  def createFunctionCall: FunctionCallExpr = FunctionCallExpr("f", List(Identifier("a", INT), Identifier("b", INT)), BrboType.INT)

  def createAssignment: Assignment = Assignment(Identifier("x", INT), Number(0))

  def createVariableDeclaration: VariableDeclaration = VariableDeclaration(Identifier("x", BrboType.INT), Number(1))

  // def createAssert: Assert = Assert(Bool(false))

  // def createAssume: Assume = Assume(Bool(false))

  def createLabeledCommand: LabeledCommand = LabeledCommand("label", Return(None))

  def createITE: ITE = ITE(Bool(b = true), Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1)))

  def createLoop: Loop = Loop(LessThan(Number(0), Identifier("x", BrboType.INT)), Block(List(Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1)))))

  def createBlock: Block = Block(List(Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1))))

  def createUse: Use = {
    val v1 = Identifier("v1", INT)
    Use(Some(5), Number(1))
  }

  def createUse2: Use = {
    val v2 = Identifier("v2", INT)
    Use(None, Number(2))
  }

  def createReset: Reset = Reset(5)

  private val useResetTest =
    """class Test {
      |  void main(int x) {
      |    use(1, 10, x > 10);
      |    reset(2, x < 10);
      |  }
      |
      |  void use(int x, int cost, boolean condition) {}
      |  void reset(int x, boolean condition) {}
      |}""".stripMargin

  val parsePredefinedFunctions: List[TestCase] = List(TestCase("useResetTest", useResetTest,
    """void main(int x)
      |{
      |  int C1 = -1;
      |  int C2 = -1;
      |  int R1 = 0;
      |  int R2 = 0;
      |  int S1 = 0;
      |  int S2 = 0;
      |  if (!((x < 10)) && !((x == 10))) R1 = R1 + 10;
      |  if (x < 10) {
      |    if (S2 < R2)
      |      S2 = R2;
      |    else
      |      ;
      |    R2 = 0;
      |    C2 = C2 + 1;
      |  }
      |}""".stripMargin))
}
