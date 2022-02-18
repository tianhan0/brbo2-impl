package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.RefinerUnitTest._
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{BrboType, CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class RefinerUnitTest extends AnyFlatSpec {
  private val debugMode = false
  private val arguments = if (debugMode) CommandLineArguments.DEBUG_MODE_ARGUMENTS else CommandLineArguments.DEFAULT_ARGUMENTS

  "Refining program 1" should "succeed" in {
    val refiner1 = new Refiner(arguments)
    testCases1.foreach({
      testCase =>
        val boundAssertion = BoundAssertion("R", LessThanOrEqualTo(Identifier("R", BrboType.INT), boundExpression1))
        val (newProgram, refinement) = refiner1.refine(program1, testCase.input.asInstanceOf[Path], boundAssertion, Set())
        StringCompare.ignoreWhitespaces(s"$refinement\n$newProgram", testCase.expectedOutput, s"Test `${testCase.name}` failed!")
    })
  }

  "Refining program 2" should "succeed" in {
    val refiner2 = new Refiner(arguments)
    testCases2.foreach({
      testCase =>
        val boundAssertion = BoundAssertion("R", LessThanOrEqualTo(Identifier("R", BrboType.INT), boundExpression2))
        val (newProgram, refinement) = refiner2.refine(program2, testCase.input.asInstanceOf[Path], boundAssertion, Set())
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

  private val boundExpression1: BrboExpr = Addition(a, Multiplication(b, ITEExpr(GreaterThan(Number(0), Subtraction(n, Number(1))), Number(0), Subtraction(n, Number(1)))))
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
      mainFunction1.ghostVariableInitializations.map(c => CFGNode(Left(c), Some(mainFunction1), CFGNode.DONT_CARE_ID)) :::
        List(
          // This path does not correspond to a path in the main function above due to the first three nodes
          // that essentially encode the pre-condition, but this is fine, because we are not parsing the path
          // (against the program). Instead, we only need to symbolically execute the path and find the suitable
          // path refinement.
          CFGNode(Right(GreaterThanOrEqualTo(n, Number(0))), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Right(GreaterThanOrEqualTo(a, Number(0))), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Right(GreaterThanOrEqualTo(b, Number(0))), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(declaration1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Right(loopCondition1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Right(iteCondition), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(assignWithA), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(use1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(increment), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Right(loopCondition1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Right(Negative(iteCondition)), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(assignWithB), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
          CFGNode(Left(use1), Some(mainFunction1), CFGNode.DONT_CARE_ID),
        ))

    List(
      TestCase("Refine Test 01", path,
        """Some(Path:
          |  (-1) int C1 = -1; [Function `main`]
          |  (-1) int R1 = 0; [Function `main`]
          |  (-1) int S1 = 0; [Function `main`]
          |  (-1) (n >= 0) [Function `main`]
          |  (-1) (a >= 0) [Function `main`]
          |  (-1) (b >= 0) [Function `main`]
          |  (-1) int i = 0; [Function `main`]
          |  (-1) (i < n) [Function `main`]
          |  (-1) (i == 0) [Function `main`]
          |  (-1) e = a; [Function `main`]
          |  (-1) if (true) reset R1 [Function `main`]
          |  (-1) if (true) use R1 e [Function `main`]
          |  (-1) i = i + 1; [Function `main`]
          |  (-1) (i < n) [Function `main`]
          |  (-1) !((i == 0)) [Function `main`]
          |  (-1) e = b; [Function `main`]
          |  (-1) if (true) reset R1 [Function `main`]
          |  (-1) if (true) use R1 e [Function `main`]
          |Splits:
          |  [010] (-1) if (true) reset R1 [Function `main`] -> if (true) reset R2
          |  [011] (-1) if (true) use R1 e [Function `main`] -> if (true) use R2 e
          |  [016] (-1) if (true) reset R1 [Function `main`] -> if (true) reset R3
          |  [017] (-1) if (true) use R1 e [Function `main`] -> if (true) use R3 e
          |Removed resets:
          |  )
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
          |        if (true) reset R2
          |        if (true) reset R3
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
      mainFunction2.ghostVariableInitializations.map(c => CFGNode(Left(c), Some(mainFunction2), CFGNode.DONT_CARE_ID)) :::
        List(
          CFGNode(Right(GreaterThanOrEqualTo(n, Number(0))), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(declaration2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(use2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(updateK1), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset3), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(use3), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(updateK2), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset4), Some(mainFunction2), CFGNode.DONT_CARE_ID),
          CFGNode(Left(use4), Some(mainFunction2), CFGNode.DONT_CARE_ID),
        ))
    List(
      TestCase("Refine Test 02", path,
        """Some(Path:
          |  (-1) int C1 = -1; [Function `main`]
          |  (-1) int R1 = 0; [Function `main`]
          |  (-1) int S1 = 0; [Function `main`]
          |  (-1) (n >= 0) [Function `main`]
          |  (-1) int k = 0; [Function `main`]
          |  (-1) if (true) reset R1 [Function `main`]
          |  (-1) if (true) use R1 1 [Function `main`]
          |  (-1) k = k + 1; [Function `main`]
          |  (-1) if (true) reset R1 [Function `main`]
          |  (-1) if (true) use R1 2 [Function `main`]
          |  (-1) k = k + 2; [Function `main`]
          |  (-1) if (true) reset R1 [Function `main`]
          |  (-1) if (true) use R1 (n - k) [Function `main`]
          |Splits:
          |  [005] (-1) if (true) reset R1 [Function `main`] -> if (true) reset R2
          |  [006] (-1) if (true) use R1 1 [Function `main`] -> if (true) use R2 1
          |  [008] (-1) if (true) reset R1 [Function `main`] -> if (true) reset R3
          |  [009] (-1) if (true) use R1 2 [Function `main`] -> if (true) use R3 2
          |  [011] (-1) if (true) reset R1 [Function `main`] -> if (true) reset R2
          |  [012] (-1) if (true) use R1 (n - k) [Function `main`] -> if (true) use R2 (n - k)
          |Removed resets:
          |  [005]: (-1) if (true) reset R1 [Function `main`])
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
          |      if (false) reset R2
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
          |      if (true) reset R2
          |    }
          |    {
          |      if (true && true) use R2 (n - k)
          |    }
          |  })""".stripMargin)
    )
  }
}