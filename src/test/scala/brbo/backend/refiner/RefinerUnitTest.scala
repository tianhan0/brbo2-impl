package brbo.backend.refiner

import brbo.TestCase
import brbo.backend.refiner.RefinerUnitTest._
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class RefinerUnitTest extends AnyFlatSpec {
  val refiner = new Refiner(program, CommandLineArguments.DEBUG_MODE_ARGUMENTS)

  "Refining a program" should "succeed" in {
    testCases.foreach({
      testCase =>
        val (newProgram, refinement) = refiner.refine(program, Some(testCase.input.asInstanceOf[Path]), boundExpression, Set())
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
  private val boundExpression: BrboExpr = Addition(a, Multiplication(b, ITEExpr(GreaterThan(Number(0), Subtraction(n, Number(1))), Number(0), Subtraction(n, Number(1)))))
  private val declaration = VariableDeclaration(i, Number(0))
  private val assignWithA = Assignment(e, a)
  private val assignWithB = Assignment(e, b)
  private val increment = Assignment(i, Addition(i, Number(1)))
  private val loopCondition = LessThan(i, n)
  private val iteCondition = Equal(i, Number(0))
  private val use = Use(Some(1), e)
  private val reset = Reset(1)

  private val mainFunction: BrboFunction = {
    val loop = {
      val ite = ITE(iteCondition, assignWithA, assignWithB)
      val loopBody = Block(List(ite, reset, use, increment))
      Loop(loopCondition, loopBody)
    }
    BrboFunction("main", VOID, List(n, a, b), Block(List(declaration, loop)), Set(1))
  }
  val program: BrboProgram = BrboProgram("Test program", mainFunction) //, None, None, List(assumeFunction, ndBoolFunction, ndIntFunction))

  val testCases: List[TestCase] = {
    val path = Path(
      mainFunction.ghostVariableInitializations.map(c => CFGNode(Left(c), mainFunction, CFGNode.DONT_CARE_ID)) :::
        List(
          // This path does not correspond to a path in the main function above due to the first three nodes
          // that essentially encode the pre-condition, but this is fine, because we are not parsing the path
          // (against the program). Instead, we only need to symbolically execute the path and find the suitable
          // path refinement.
          CFGNode(Right(GreaterThanOrEqualTo(n, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Right(GreaterThanOrEqualTo(a, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Right(GreaterThanOrEqualTo(b, Number(0))), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(declaration), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Right(loopCondition), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Right(iteCondition), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(assignWithA), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(increment), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Right(loopCondition), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Right(Negative(iteCondition)), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(assignWithB), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(reset), mainFunction, CFGNode.DONT_CARE_ID),
          CFGNode(Left(use), mainFunction, CFGNode.DONT_CARE_ID),
        ))

    List(
      TestCase("Refine Test 01", path,
        """Some(Path:
          |  int C1 = -1; [Function `main`]
          |  int R1 = 0; [Function `main`]
          |  int S1 = 0; [Function `main`]
          |  (n >= 0) [Function `main`]
          |  (a >= 0) [Function `main`]
          |  (b >= 0) [Function `main`]
          |  int i = 0; [Function `main`]
          |  (i < n) [Function `main`]
          |  (i == 0) [Function `main`]
          |  e = a; [Function `main`]
          |  if (true) reset R1 [Function `main`]
          |  if (true) use R1 e [Function `main`]
          |  i = i + 1; [Function `main`]
          |  (i < n) [Function `main`]
          |  !((i == 0)) [Function `main`]
          |  e = b; [Function `main`]
          |  if (true) reset R1 [Function `main`]
          |  if (true) use R1 e [Function `main`]
          |Splits:
          |  10: if (true) reset R2
          |  11: if (true) use R2 e
          |  16: if (true) reset R3
          |  17: if (true) use R3 e
          |Removed resets:
          |  )
          |Some(Program name: `Test program`
          |Most precise bound: `None`
          |Less precise bound: `None`
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
}