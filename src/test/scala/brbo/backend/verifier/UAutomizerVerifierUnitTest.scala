package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.common.BrboType.{INT, VOID}
import brbo.common.CommandLineArguments
import brbo.common.CommandLineArguments._
import brbo.common.ast.BrboExprUtils.{greaterThan, lessThanOrEqualTo}
import brbo.common.ast._
import org.scalatest.flatspec.AnyFlatSpec

class UAutomizerVerifierUnitTest extends AnyFlatSpec {
  val arguments = new CommandLineArguments
  arguments.initialize(
    TEST_MODE,
    debugMode = DEFAULT_DEBUG_MODE,
    "",
    printVerifierInputs = DEFAULT_PRINT_VERIFIER_INPUTS,
    verifierTimeout = 10,
    printCFG = DEFAULT_PRINT_CFG,
    maxGroups = DEFAULT_MAX_GROUPS,
    verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
    relationalPredicates = DEFAULT_RELATIONAL_PREDICATES,
    maxIterations = DEFAULT_MAX_ITERATIONS,
    assertionTag = DEFAULT_ASSERTION_TAG,
    abstractDomain = DEFAULT_ABSTRACT_DOMAIN,
    maxPathLength = DEFAULT_MAX_PATH_LENGTH,
    checkWithZ3 = DEFAULT_CHECK_WITH_Z3,
    assumePositiveInputs = DEFAULT_ASSUME_POSITIVE_INPUTS,
    widenThreshold = DEFAULT_WIDEN_THRESHOLD,
    numberOfThreads = DEFAULT_NUMBER_OF_THREADS,
  )

  "Parsing counterexample paths" should "be correct" in {
    UAutomizerVerifierUnitTest.testCases.foreach({
      testCase =>
      // val verifier = new UAutomizerVerifier(arguments)
      // val result = verifier.verify(testCase.input.asInstanceOf[BrboProgram])
      // StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"Test `${testCase.name}` failed")
    })
  }
}

object UAutomizerVerifierUnitTest {
  val testCases: List[TestCase] = {
    val i = Identifier("i", INT)
    val n = Identifier("n", INT)
    val a = Identifier("a", INT)
    val b = Identifier("b", INT)
    val R = Identifier("R", INT)
    val statement1 = VariableDeclaration(i, Number(0))
    val statement2 = VariableDeclaration(R, Number(0))
    val statement3 = PreDefinedFunctions.createAssume(greaterThan(n, Number(0)))
    val statement5 = FunctionCall(FunctionCallExpr("ndBool", Nil, INT)) // To test parsing counterexample paths when involving function calls
    val statement6 = {
      val e = Identifier("e", INT)
      val statement1 = VariableDeclaration(e, Number(0))
      val statement2 = ITE(LessThan(i, Number(1)), Assignment(e, a), Assignment(e, b))
      val statement3 = Assignment(R, Addition(R, e))
      val statement4 = Assignment(i, Addition(i, Number(1)))
      Loop(LessThan(i, n), Block(List(statement1, statement2, statement3, statement4)))
    }
    val assertionTrue = PreDefinedFunctions.createAssert(Or(lessThanOrEqualTo(R, a), lessThanOrEqualTo(R, Addition(a, Multiplication(Subtraction(n, Number(1)), b)))))
    val assertionFalse = PreDefinedFunctions.createAssert(lessThanOrEqualTo(R, a))

    val test01 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement5, statement6, assertionTrue)), Set())
      BrboProgram("test01", function, Nil, PreDefinedFunctions.allFunctionsList)
    }
    val test01Expected = """VerifierResult(UNKNOWN_RESULT,Set())"""

    val test02 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement5, statement6, assertionFalse)), Set())
      BrboProgram("test02", function, Nil, PreDefinedFunctions.allFunctionsList)
    }
    val test02Expected =
      """VerifierResult(FALSE_RESULT,Set(Path:
        |  [000] (2) int i = 0; [fun `main`]
        |  [001] (3) int R = 0; [fun `main`]
        |  [002] (-1) Call function `assume` with `(n > 0)` [fun `main`]
        |  [003] (21) !(!(cond)) [fun `assume`]
        |  [004] (20) [Function Exit] [fun `assume`]
        |  [005] (-1) Call function `ndBool` no arguments [fun `main`]
        |  [006] (-1) Call function `ndInt` no arguments [fun `ndBool`]
        |  [007] (25) return __VERIFIER_nondet_int(); [fun `ndInt`]
        |  [008] (27) int x = ndInt(); [fun `ndBool`]
        |  [009] (28) (x > 0) [fun `ndBool`]
        |  [010] (29) return true; [fun `ndBool`]
        |  [011] (6) (i < n) [fun `main`]
        |  [012] (8) int e = 0; [fun `main`]
        |  [013] (9) (i < 1) [fun `main`]
        |  [014] (10) e = a; [fun `main`]
        |  [015] (12) R = R + e; [fun `main`]
        |  [016] (13) i = i + 1; [fun `main`]
        |  [017] (6) (i < n) [fun `main`]
        |  [018] (8) int e = 0; [fun `main`]
        |  [019] (9) !((i < 1)) [fun `main`]
        |  [020] (11) e = b; [fun `main`]
        |  [021] (12) R = R + e; [fun `main`]
        |  [022] (13) i = i + 1; [fun `main`]
        |  [023] (6) !((i < n)) [fun `main`]
        |  [024] (-1) Call function `assert` with `(R <= a)` [fun `main`]
        |  [025] (16) !(cond) [fun `assert`]
        |  [026] (17) ERROR: __VERIFIER_error(); [fun `assert`]
        |  [027] (19) return; [fun `assert`]))""".stripMargin

    val test03 = {
      val n = Identifier("n", INT)
      val reset1 = Reset(2, greaterThan(n, Number(0)))
      val R2 = reset1.resourceVariable
      val reset2 = Reset(2)
      val use1 = Use(Some(2), Number(1), greaterThan(n, Number(1)))
      val use2 = Use(Some(2), Number(2))
      val assertion = PreDefinedFunctions.createAssert(lessThanOrEqualTo(R2, Number(1)))
      val function = BrboFunction("main", VOID, List(n), Block(List(reset1, use1, reset2, use2, assertion)), Set[Int](2))
      BrboProgram("test03", function, Nil, PreDefinedFunctions.allFunctionsList)
    }
    val test03Expected =
      """VerifierResult(FALSE_RESULT,Set(Path:
        |  [000] (2) int C2 = -1; [fun `main`]
        |  [001] (3) int R2 = 0; [fun `main`]
        |  [002] (4) int S2 = 0; [fun `main`]
        |  [003] (-1) if (n > 0) reset R2 [fun `main`]
        |  [004] (-1) if (n > 1) use R2 1 [fun `main`]
        |  [005] (-1) if (true) reset R2 [fun `main`]
        |  [006] (-1) if (true) use R2 2 [fun `main`]
        |  [007] (-1) Call function `assert` with `(R2 <= 1)` [fun `main`]
        |  [008] (27) !(cond) [fun `assert`]
        |  [009] (28) ERROR: __VERIFIER_error(); [fun `assert`]
        |  [010] (30) return; [fun `assert`]))""".stripMargin

    List[TestCase](
      TestCase("Must be unknown", test01, test01Expected), // This must time out!
      TestCase("Must fail", test02, test02Expected),
      TestCase("Identify uses and resets", test03, test03Expected),
    )
  }
}