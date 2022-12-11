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

  "Parsing Java programs into Brbo ASTs" should "be correct" in {
    BrboAstUnitTest.parsingAstTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        StringCompare.ignoreWhitespaces(targetProgram.program.mainFunction.printToC(0), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Printing a program to Java" should "be correct" in {
    BrboAstUnitTest.printToJavaTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        StringCompare.ignoreWhitespaces(targetProgram.program.printToJava(), testCase.expectedOutput, s"${testCase.name} failed")
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
      TestCase("ITE", createITE,
        """  if (true)
          |  {
          |    x = 0;
          |  }
          |  else
          |  {
          |    x = 1;
          |  }""".stripMargin),
      TestCase("Loop", createLoop, "  while (0 < x)\n  {\n    x = 0;\n    x = 1;\n  }"),
      TestCase("Block", createBlock, "  {\n    x = 0;\n    x = 1;\n  }"),
      TestCase("Use", createUse, "  R5 = R5 + 1;"),
      TestCase("Use 2", createUse2, "  R = R + 2;"),
      TestCase("Reset", createReset,
        """  if (S5 < R5)
          |  {
          |    S5 = R5;
          |  }
          |  else
          |  {
          |    ;
          |  }
          |  R5 = 0;
          |  C5 = C5 + 1;""".stripMargin)
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
      |    use(3, 100);
      |    reset(4);
      |  }
      |
      |  void use(int x, int cost, boolean condition) {}
      |  void use(int x, int cost) {}
      |  void reset(int x, boolean condition) {}
      |  void reset(int x) {}
      |}""".stripMargin

  private val arrayInputTest =
    """class Test {
      |  void main(int[] x) {
      |  }
      |}""".stripMargin

  private val arrayReadTest =
    """class Test {
      |  void main(int[] x) {
      |    arrayRead(x, 0);
      |    int y = arrayRead(x, 10);
      |  }
      |
      |  int arrayRead(int[] x, int index) { return 0; }
      |}""".stripMargin

  private val arrayLengthTest =
    """class Test {
      |  void main(int[] x) {
      |    arrayLength(x);
      |    int y = arrayLength(x);
      |  }
      |
      |  int arrayLength(int[] x) { return 0; }
      |}""".stripMargin

  private val arraySumTest =
    """class Test {
      |  void main(int[] x) {
      |    arraySum(x);
      |    int y = arraySum(x);
      |  }
      |
      |  int arraySum(int[] x) { return 0; }
      |}""".stripMargin

  private val upperBoundTest =
    """class Test {
      |  void main(int x) {
      |    upperBound(0, "tag", x + 1);
      |  }
      |
      |  int upperBound(int group, String index, int bound) { return 0; }
      |}""".stripMargin

  private val resourceVariableTest =
    """class Test {
      |  void main(int x) {
      |    int R = 0;
      |    R = R + 22;
      |  }
      |}""".stripMargin

  val parsingAstTests: List[TestCase] = List(
    TestCase("useResetTest", useResetTest,
      """void main(int x)
        |{
        |  int C1 = 0;
        |  int C2 = 0;
        |  int C3 = 0;
        |  int C4 = 0;
        |  int R1 = 0;
        |  int R2 = 0;
        |  int R3 = 0;
        |  int R4 = 0;
        |  int S1 = -2147483648;
        |  int S2 = -2147483648;
        |  int S3 = -2147483648;
        |  int S4 = -2147483648;
        |  if (!((x < 10)) && !((x == 10)))
        |  {
        |    R1 = R1 + 10;
        |  }
        |  else
        |  {
        |    ;
        |  }
        |  if (x < 10)
        |  {
        |    if (S2 < R2)
        |    {
        |      S2 = R2;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    R2 = 0;
        |    C2 = C2 + 1;
        |  }
        |  else
        |  {
        |    ;
        |  }
        |  R3 = R3 + 100;
        |  if (S4 < R4)
        |  {
        |    S4 = R4;
        |  }
        |  else
        |  {
        |    ;
        |  }
        |  R4 = 0;
        |  C4 = C4 + 1;
        |}
        |
        |""".stripMargin),
    TestCase("arrayInputTest", arrayInputTest,
      """void main(int x)
        |{
        |
        |}""".stripMargin),
    TestCase("arrayReadTest", arrayReadTest,
      """void main(int x)
        |{
        |  arrayRead(x, 0);
        |  int y = arrayRead(x, 10);
        |}""".stripMargin),
    TestCase("arrayLengthTest", arrayLengthTest,
      """void main(int x)
        |{
        |  arrayLength(x);
        |  int y = arrayLength(x);
        |}""".stripMargin),
    TestCase("arraySumTest", arraySumTest,
      """void main(int x)
        |{
        |  arraySum(x);
        |  int y = arraySum(x);
        |}""".stripMargin),
    TestCase("upperBoundTest", upperBoundTest,
      """void main(int x)
        |{
        |  ;
        |}""".stripMargin),
    TestCase("resourceVariableTest", resourceVariableTest,
      """void main(int x)
        |{
        |  int R = 0;
        |  R = R + 22;
        |}
        |""".stripMargin),
  )

  private val printToJavaTest1 =
    """abstract class Test {
      |  void main(int x, int[] array) {
      |    use(1, 10, x > 10);
      |    reset(2, x < 10);
      |    use(3, 100);
      |    reset(4);
      |    int R = 0;
      |    R = R + 1011;
      |    int a1 = arrayRead(array, 0);
      |    int a2 = arraySum(array);
      |    int a3 = arrayLength(array);
      |    int a4 = ndInt();
      |    int a5 = ndInt2(12, 34);
      |    boolean a6 = ndBool();
      |    mostPreciseBound(R + arraySum(array) > 10);
      |    lessPreciseBound(R + arraySum(array) < 10);
      |  }
      |
      |  void use(int x, int cost, boolean condition) {}
      |  void use(int x, int cost) {}
      |  void reset(int x, boolean condition) {}
      |  void reset(int x) {}
      |  int arrayRead(int[] x, int index) { return 0; }
      |  int arraySum(int[] x) { return 0; }
      |  int arrayLength(int[] x) { return 0; }
      |  public abstract int ndInt();
      |  public abstract int ndInt2(int lower, int upper);
      |  public abstract boolean ndBool();
      |  public abstract void mostPreciseBound(boolean assertion);
      |  public abstract void lessPreciseBound(boolean assertion);
      |}""".stripMargin

  val printToJavaTests: List[TestCase] = List(
    TestCase("printToJavaTest1", printToJavaTest1,
      """abstract class Test {
        |  void main(int x, int array)
        |
        |  {
        |    int C1 = -1;
        |    int C2 = -1;
        |    int C3 = -1;
        |    int C4 = -1;
        |    int D1 = 0;
        |    int D1p = 0;
        |    int D2 = 0;
        |    int D2p = 0;
        |    int D3 = 0;
        |    int D3p = 0;
        |    int D4 = 0;
        |    int D4p = 0;
        |    lessPreciseBound((((((0 + D1) + D2) + D3) + D4) + arraySum(array)) < 10);
        |    mostPreciseBound(!(((((((0 + D1) + D2) + D3) + D4) + arraySum(array)) < 10)) && !(((((((0 + D1) + D2) + D3) + D4) + arraySum(array)) == 10)));
        |    if (!((x < 10)) && !((x == 10)))
        |    {
        |      D1 = D1 + 10;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    if (x < 10)
        |    {
        |      if (D2p < D2)
        |      {
        |        D2p = D2;
        |      }
        |      else
        |      {
        |        ;
        |      }
        |      D2 = 0;
        |      C2 = C2 + 1;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    D3 = D3 + 100;
        |    if (D4p < D4)
        |    {
        |      D4p = D4;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    D4 = 0;
        |    C4 = C4 + 1;
        |    int R = 0;
        |    ;
        |    int a1 = arrayRead(array, 0);
        |    int a2 = arraySum(array);
        |    int a3 = arrayLength(array);
        |    int a4 = ndInt();
        |    int a5 = ndInt2(12, 34);
        |    int a6 = ndBool();
        |    ;
        |    ;
        |  }
        |  public abstract int ndInt();
        |  public abstract int ndInt2(int lower, int upper);
        |  public abstract boolean ndBool();
        |  public abstract void assume(boolean expression);
        |  public abstract void mostPreciseBound(boolean assertion);
        |  public abstract void lessPreciseBound(boolean assertion);
        |  public abstract void resetPlaceHolder();
        |}
        |""".stripMargin)
  )
}
