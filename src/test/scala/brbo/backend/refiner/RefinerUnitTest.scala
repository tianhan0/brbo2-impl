package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.RefinerUnitTest._
import brbo.backend.verifier.SymbolicExecution
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{BrboType, CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class RefinerUnitTest extends AnyFlatSpec {
  private val debugMode = false
  private val arguments = if (debugMode) CommandLineArguments.DEBUG_MODE_ARGUMENTS else CommandLineArguments.DEFAULT_ARGUMENTS

  "Refining program 1 with symbolic execution" should "succeed" in {
    val refiner1 = new Refiner(arguments)
    testCases1.foreach({
      testCase =>
        val boundAssertion = BoundAssertion("R", LessThanOrEqualTo(Identifier("R", BrboType.INT), boundExpression1), tag = "IrrelevantTag")
        val symbolicExecution = new SymbolicExecution(program1.mainFunction.parameters, debugMode = false)
        val (newProgram, refinement) = refiner1.refine(program1, testCase.input.asInstanceOf[Path], boundAssertion, Set(), symbolicExecution)
        StringCompare.ignoreWhitespaces(s"$refinement\n$newProgram", testCase.expectedOutput, s"Test `${testCase.name}` failed!")
    })
  }

  "Refining program 2 with symbolic execution" should "succeed" in {
    val refiner2 = new Refiner(arguments)
    testCases2.foreach({
      testCase =>
        val boundAssertion = BoundAssertion("R", LessThanOrEqualTo(Identifier("R", BrboType.INT), boundExpression2), tag = "IrrelevantTag")
        val symbolicExecution = new SymbolicExecution(program1.mainFunction.parameters, debugMode = false)
        val (newProgram, refinement) = refiner2.refine(program2, testCase.input.asInstanceOf[Path], boundAssertion, Set(), symbolicExecution)
        StringCompare.ignoreWhitespaces(s"$refinement\n$newProgram", testCase.expectedOutput, s"Test `${testCase.name}` failed!")
    })
  }
}

object RefinerUnitTest {
  private val i: Identifier = Identifier("i", INT)
  private val n: Identifier = Identifier("n", INT)
  private val a: Identifier = Identifier("a", INT)
  private val b: Identifier = Identifier("b", INT)
  private val e: Identifier = Identifier("e", INT)
  private val k: Identifier = Identifier("k", INT)

  private val boundExpression1: BrboExpr =
    Addition(
      a,
      Multiplication(
        b,
        ITEExpr(GreaterThanOrEqualTo(Subtraction(n, Number(1)), Number(0)), Subtraction(n, Number(1)), Number(0))
      )
    )
  private val boundExpression2: BrboExpr = n

  private val declaration1 = VariableDeclaration(i, Number(0))
  private val assignWithA = Assignment(e, a)
  private val assignWithB = Assignment(e, b)
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val loopCondition1 = LessThan(i, n)
  private val iteCondition = Equal(i, Number(0))
  private val use1 = Use(Some(1), e)
  private val reset1 = Reset(1)

  private val declaration2 = VariableDeclaration(k, Number(0))
  private val reset2 = Reset(1)
  private val use2 = Use(Some(1), Number(1))
  private val updateK1 = Assignment(k, Addition(k, Number(1)))
  private val reset3 = Reset(1)
  private val use3 = Use(Some(1), Number(2))
  private val updateK2 = Assignment(k, Addition(k, Number(2)))
  private val reset4 = Reset(1)
  private val use4 = Use(Some(1), Subtraction(n, k))

  private val mainFunction1: BrboFunction = {
    val loop = {
      val ite = ITE(iteCondition, assignWithA, assignWithB)
      val loopBody = Block(List(ite, reset1, use1, increment))
      Loop(loopCondition1, loopBody)
    }
    BrboFunction("main", VOID, List(n, a, b), Block(List(declaration1, loop)), Set(1))
  }
  private val program1: BrboProgram = BrboProgram("Test program", mainFunction1)

  private val mainFunction2: BrboFunction = {
    BrboFunction("main", VOID, List(n), Block(List(declaration2, reset2, use2, updateK1, reset3, use3, updateK2, reset4, use4)), Set(1))
  }
  private val program2: BrboProgram = BrboProgram("Test program", mainFunction2)

  val testCases1: List[TestCase] = {
    val path = Path(
      mainFunction1.ghostVariableInitializations.map(c => CFGNode(c, Some(mainFunction1), CFGNode.DONT_CARE_ID)) :::
        List(
          // This path does not correspond to a path in the main function above due to the first three nodes
          // that essentially encode the pre-condition, but this is fine, because we are not parsing the path
          // (against the program). Instead, we only need to symbolically execute the path and find the suitable
          // path refinement.
          CFGNode((GreaterThanOrEqualTo(n, Number(0))), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((GreaterThanOrEqualTo(a, Number(0))), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((GreaterThanOrEqualTo(b, Number(0))), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((declaration1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((loopCondition1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((iteCondition), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((assignWithA), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((reset1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((use1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((increment), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((loopCondition1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((Negation(iteCondition)), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((assignWithB), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((reset1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode((use1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
        ))

    List(
      TestCase("Refine Test 01", path,
        """Some(Path:
          |  [000] (-1) int C1 = -1; [fun `main`]
          |  [001] (-1) int R1 = 0; [fun `main`]
          |  [002] (-1) int S1 = 0; [fun `main`]
          |  [003] (-1) (n >= 0) [fun `main`]
          |  [004] (-1) (a >= 0) [fun `main`]
          |  [005] (-1) (b >= 0) [fun `main`]
          |  [006] (-1) int i = 0; [fun `main`]
          |  [007] (-1) (i < n) [fun `main`]
          |  [008] (-1) (i == 0) [fun `main`]
          |  [009] (-1) e = a; [fun `main`]
          |  [010] (-1) if (true) reset R1 [fun `main`]
          |  [011] (-1) if (true) use R1 e [fun `main`]
          |  [012] (-1) i = i + 1; [fun `main`]
          |  [013] (-1) (i < n) [fun `main`]
          |  [014] (-1) !((i == 0)) [fun `main`]
          |  [015] (-1) e = b; [fun `main`]
          |  [016] (-1) if (true) reset R1 [fun `main`]
          |  [017] (-1) if (true) use R1 e [fun `main`]
          |Splits:
          |  [010] (-1) if (true) reset R1 [fun `main`] -> if (true) reset R2
          |  [011] (-1) if (true) use R1 e [fun `main`] -> if (true) use R2 e
          |  [016] (-1) if (true) reset R1 [fun `main`] -> if (true) reset R3
          |  [017] (-1) if (true) use R1 e [fun `main`] -> if (true) use R3 e
          |Removed resets:
          |  [016]: (-1) if (true) reset R1 [fun `main`])
          |Some(Program name: `Test program`
          |Global assertions to verify: ``
          |void main(int n, int a, int b)
          |  {
          |    int C2 = -1;
          |    int C3 = -1;
          |    int R2 = 0;
          |    int R3 = 0;
          |    int S2 = 0;
          |    int S3 = 0;
          |    int i = 0;
          |    while (i < n)
          |    {
          |      if (i == 0)
          |        e = a;
          |      else
          |        e = b;
          |      {
          |        if (false) reset R3
          |        if (true) reset R2
          |      }
          |      {
          |        if (((0 - i) >= 0) && true) use R2 e
          |        if ((i > 0) && true) use R3 e
          |      }
          |      i = i + 1;
          |    }
          |  })""".stripMargin)
    )
  }

  val testCases2: List[TestCase] = {
    val path = Path(
      mainFunction2.ghostVariableInitializations.map(c => CFGNode(c, Some(mainFunction2), CFGNode.DONT_CARE_ID)) :::
        List(
          CFGNode((GreaterThanOrEqualTo(n, Number(0))), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((declaration2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((reset2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((use2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((updateK1), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((reset3), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((use3), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((updateK2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((reset4), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode((use4), Some(mainFunction2), CFGNode.DONT_CARE_ID),
        ))
    List(
      TestCase("Refine Test 02", path,
        """Some(Path:
          |  [000] (-1) int C1 = -1; [fun `main`]
          |  [001] (-1) int R1 = 0; [fun `main`]
          |  [002] (-1) int S1 = 0; [fun `main`]
          |  [003] (-1) (n >= 0) [fun `main`]
          |  [004] (-1) int k = 0; [fun `main`]
          |  [005] (-1) if (true) reset R1 [fun `main`]
          |  [006] (-1) if (true) use R1 1 [fun `main`]
          |  [007] (-1) k = k + 1; [fun `main`]
          |  [008] (-1) if (true) reset R1 [fun `main`]
          |  [009] (-1) if (true) use R1 2 [fun `main`]
          |  [010] (-1) k = k + 2; [fun `main`]
          |  [011] (-1) if (true) reset R1 [fun `main`]
          |  [012] (-1) if (true) use R1 (n - k) [fun `main`]
          |Splits:
          |  [005] (-1) if (true) reset R1 [fun `main`] -> if (true) reset R2
          |  [006] (-1) if (true) use R1 1 [fun `main`] -> if (true) use R2 1
          |  [008] (-1) if (true) reset R1 [fun `main`] -> if (true) reset R3
          |  [009] (-1) if (true) use R1 2 [fun `main`] -> if (true) use R3 2
          |  [011] (-1) if (true) reset R1 [fun `main`] -> if (true) reset R3
          |  [012] (-1) if (true) use R1 (n - k) [fun `main`] -> if (true) use R3 (n - k)
          |Removed resets:
          |  [011]: (-1) if (true) reset R1 [fun `main`])
          |Some(Program name: `Test program`
          |Global assertions to verify: ``
          |void main(int n)
          |  {
          |    int C2 = -1;
          |    int C3 = -1;
          |    int R2 = 0;
          |    int R3 = 0;
          |    int S2 = 0;
          |    int S3 = 0;
          |    int k = 0;
          |    {
          |      if (true) reset R2
          |    }
          |    {
          |      if (true && true) use R2 1
          |    }
          |    k = k + 1;
          |    {
          |      if (true) reset R3
          |    }
          |    {
          |      if (true && true) use R3 2
          |    }
          |    k = k + 2;
          |    {
          |      if (false) reset R3
          |    }
          |    {
          |      if (true && true) use R3 (n - k)
          |    }
          |  })""".stripMargin)
    )
  }
}