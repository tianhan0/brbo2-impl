package brbo.common.ast

import brbo.TestCase
import brbo.common.BrboType.INT
import brbo.common.BrboType
import brbo.common.string.StringCompare
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
    // assert(UndefinedFunction("f") != UndefinedFunction("f"))
    assert(createUse != createUse)
    assert(createUse2 != createUse2)
    assert(createReset != createReset)
  }

  "Pretty-printing BrboAst to C statements" should "be correct" in {
    BrboAstUnitTest.prettyPrintToCUnitTest.foreach({
      testCase =>
        assert(StringCompare.compareLiteral(testCase.input.asInstanceOf[BrboAst].prettyPrintToC(2), testCase.expectedOutput, s"${testCase.name} failed!"))
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
      TestCase("Reset", createReset, "  if (true) {\n    if (S5 < R5)\n      S5 = R5;\n    else\n      ;\n    R5 = 0;\n    C5 = C5 + 1;\n  }")
    )

  def createContinue: Continue = Continue()

  def createBreak: Break = Break()

  def createSkip: Skip = Skip()

  def createReturn: Return = Return(Some(Identifier("x", INT)))

  def createFunctionCall: FunctionCall = FunctionCall(FunctionCallExpr("f", List(Identifier("a", INT), Identifier("b", INT)), BrboType.INT))

  def createAssignment: Assignment = Assignment(Identifier("x", INT), Number(0))

  def createVariableDeclaration: VariableDeclaration = VariableDeclaration(Identifier("x", BrboType.INT), Number(1))

  // def createAssert: Assert = Assert(Bool(false))

  // def createAssume: Assume = Assume(Bool(false))

  def createLabeledCommand: LabeledCommand = LabeledCommand("label", Return(None))

  def createITE: ITE = ITE(Bool(true), Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1)))

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
}
