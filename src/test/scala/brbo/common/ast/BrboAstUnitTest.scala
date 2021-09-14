package brbo.common.ast

import brbo.common.TypeUtils.BrboType
import brbo.common.TypeUtils.BrboType.INT
import brbo.{StringCompare, TestCase}
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
    assert(UndefinedFunction("f") != UndefinedFunction("f"))
  }

  BrboAstUnitTest.prettyPrintToCUnitTest.foreach({
    testCase =>
      StringCompare.compareLiteral(testCase.input.asInstanceOf[BrboAst].prettyPrintToC(2), testCase.expectedOutput, s"${testCase.name} failed!")
  })
}

object BrboAstUnitTest {
  val prettyPrintToCUnitTest: List[TestCase] =
    List[TestCase](
      TestCase("Continue", createContinue, "  continue;"),
      TestCase("Break", createBreak, "  break;"),
      TestCase("Skip", createSkip, "  ;"),
      TestCase("Return", createReturn, "  return x;"),
      TestCase("FunctionCall", createFunctionCall, "  x = f(a, b);"),
      TestCase("Assignment", createAssignment, "  x = 0;"),
      TestCase("VariableDeclaration", createVariableDeclaration, "  int x = 1;"),
      // TestCase("Assert", createAssert, "  assert(false);"),
      // TestCase("Assume", createAssume, "  assume(false);"),
      TestCase("LabeledCommand", createLabeledCommand, "  label: return;"),
      TestCase("ITE", createITE, "  if (true)\n    x = 0;\n  else\n    x = 1;"),
      TestCase("Loop", createLoop, "  while (0 < x)\n  {\n    x = 0;\n    x = 1;\n  }"),
      TestCase("Block", createBlock, "  {\n    x = 0;\n    x = 1;\n  }")
    )

  def createContinue: Continue = Continue()

  def createBreak: Break = Break()

  def createSkip: Skip = Skip()

  def createReturn: Return = Return(Some(Identifier("x", INT)))

  def createFunctionCall: FunctionCall = FunctionCall(Some(Identifier("x", INT)), FunctionCallExpr("f", List(Identifier("a", INT), Identifier("b", INT)), BrboType.INT))

  def createAssignment: Assignment = Assignment(Identifier("x", INT), Number(0))

  def createVariableDeclaration: VariableDeclaration = VariableDeclaration(Identifier("x", BrboType.INT), Number(1))

  // def createAssert: Assert = Assert(Bool(false))

  // def createAssume: Assume = Assume(Bool(false))

  def createLabeledCommand: LabeledCommand = LabeledCommand("label", Return(None))

  def createITE: ITE = ITE(Bool(true), Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1)))

  def createLoop: Loop = Loop(LessThan(Number(0), Identifier("x", BrboType.INT)), Block(List(Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1)))))

  def createBlock: Block = Block(List(Assignment(Identifier("x", INT), Number(0)), Assignment(Identifier("x", INT), Number(1))))
}
