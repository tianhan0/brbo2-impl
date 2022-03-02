package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.TEST_MODE
import brbo.common.BrboType.{INT, VOID}
import brbo.common.CommandLineArguments._
import brbo.common.ast._
import brbo.common.{CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class UAutomizerVerifierUnitTest extends AnyFlatSpec {
  val arguments = new CommandLineArguments
  arguments.initialize(
    TEST_MODE,
    debugMode = false,
    "",
    printVerifierInputs = false,
    verifierTimeout = 10,
    printCFG = false,
    generateSynthetic = 0,
    maxGroups = DEFAULT_MAX_GROUPS,
    verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
    relationalPredicates = false,
    maxIterations = DEFAULT_MAX_ITERATIONS,
    assertionTag = DEFAULT_ASSERTION_TAG,
    abstractDomain = DEFAULT_ABSTRACT_DOMAIN,
    maxPathLength = DEFAULT_MAX_PATH_LENGTH
  )

  "Parsing counterexample paths" should "be correct" in {
    UAutomizerVerifierUnitTest.testCases.foreach({
      testCase =>
        // val verifier = new UAutomizerVerifier(CommandLineArguments.DEBUG_MODE_ARGUMENTS)
        val verifier = new UAutomizerVerifier(arguments)
        val result = verifier.verify(testCase.input.asInstanceOf[BrboProgram])
        StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"Test `${testCase.name}` failed")
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
    val statement3 = PreDefinedFunctions.createAssume(GreaterThan(n, Number(0)))
    val statement5 = FunctionCall(FunctionCallExpr("ndBool", Nil, INT)) // To test parsing counterexample paths when involving function calls
    val statement6 = {
      val e = Identifier("e", INT)
      val statement1 = VariableDeclaration(e, Number(0))
      val statement2 = ITE(LessThan(i, Number(1)), Assignment(e, a), Assignment(e, b))
      val statement3 = Assignment(R, Addition(R, e))
      val statement4 = Assignment(i, Addition(i, Number(1)))
      Loop(LessThan(i, n), Block(List(statement1, statement2, statement3, statement4)))
    }
    val assertionTrue = PreDefinedFunctions.createAssert(Or(LessThanOrEqualTo(R, a), LessThanOrEqualTo(R, Addition(a, Multiplication(Subtraction(n, Number(1)), b)))))
    val assertionFalse = PreDefinedFunctions.createAssert(LessThanOrEqualTo(R, a))

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
        |  [000] (2) int i = 0; [Function `main`]
        |  [001] (3) int R = 0; [Function `main`]
        |  [002] (-1) Call function `assume` with `(n > 0)` [Function `main`]
        |  [003] (21) !(!(cond)) [Function `assume`]
        |  [004] (20) [Function Exit] [Function `assume`]
        |  [005] (-1) Call function `ndBool` no arguments [Function `main`]
        |  [006] (-1) Call function `ndInt` no arguments [Function `ndBool`]
        |  [007] (25) return __VERIFIER_nondet_int(); [Function `ndInt`]
        |  [008] (27) int x = ndInt(); [Function `ndBool`]
        |  [009] (28) (x > 0) [Function `ndBool`]
        |  [010] (29) return true; [Function `ndBool`]
        |  [011] (6) (i < n) [Function `main`]
        |  [012] (8) int e = 0; [Function `main`]
        |  [013] (9) (i < 1) [Function `main`]
        |  [014] (10) e = a; [Function `main`]
        |  [015] (12) R = R + e; [Function `main`]
        |  [016] (13) i = i + 1; [Function `main`]
        |  [017] (6) (i < n) [Function `main`]
        |  [018] (8) int e = 0; [Function `main`]
        |  [019] (9) !((i < 1)) [Function `main`]
        |  [020] (11) e = b; [Function `main`]
        |  [021] (12) R = R + e; [Function `main`]
        |  [022] (13) i = i + 1; [Function `main`]
        |  [023] (6) !((i < n)) [Function `main`]
        |  [024] (-1) Call function `assert` with `(R <= a)` [Function `main`]
        |  [025] (16) !(cond) [Function `assert`]
        |  [026] (17) ERROR: __VERIFIER_error(); [Function `assert`]
        |  [027] (19) return; [Function `assert`]))""".stripMargin

    val test03 = {
      val n = Identifier("n", INT)
      val reset1 = Reset(2, GreaterThan(n, Number(0)))
      val R2 = reset1.resourceVariable
      val reset2 = Reset(2)
      val use1 = Use(Some(2), Number(1), GreaterThan(n, Number(1)))
      val use2 = Use(Some(2), Number(2))
      val assertion = PreDefinedFunctions.createAssert(LessThanOrEqualTo(R2, Number(1)))
      val function = BrboFunction("main", VOID, List(n), Block(List(reset1, use1, reset2, use2, assertion)), Set[Int](2))
      BrboProgram("test03", function, Nil, PreDefinedFunctions.allFunctionsList)
    }
    val test03Expected =
      """VerifierResult(FALSE_RESULT,Set(Path:
        |  [000] (2) int C2 = -1; [Function `main`]
        |  [001] (3) int R2 = 0; [Function `main`]
        |  [002] (4) int S2 = 0; [Function `main`]
        |  [003] (-1) if (n > 0) reset R2 [Function `main`]
        |  [004] (-1) if (n > 1) use R2 1 [Function `main`]
        |  [005] (-1) if (true) reset R2 [Function `main`]
        |  [006] (-1) if (true) use R2 2 [Function `main`]
        |  [007] (-1) Call function `assert` with `(R2 <= 1)` [Function `main`]
        |  [008] (27) !(cond) [Function `assert`]
        |  [009] (28) ERROR: __VERIFIER_error(); [Function `assert`]
        |  [010] (30) return; [Function `assert`]))""".stripMargin

    List[TestCase](
      TestCase("Must be unknown", test01, test01Expected), // This must time out!
      TestCase("Must fail", test02, test02Expected),
      TestCase("Identify uses and resets", test03, test03Expected),
    )
  }
}