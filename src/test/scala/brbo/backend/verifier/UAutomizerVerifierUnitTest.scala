package brbo.backend.verifier

import brbo.TestCase
import brbo.backend.verifier.AmortizationMode.UNKNOWN_MODE
import brbo.common.BrboType.{INT, VOID}
import brbo.common.CommandLineArguments.DEFAULT_MAX_GROUPS
import brbo.common.ast._
import brbo.common.{CommandLineArguments, StringCompare}
import org.scalatest.flatspec.AnyFlatSpec

class UAutomizerVerifierUnitTest extends AnyFlatSpec {
  val arguments = new CommandLineArguments
  arguments.initialize(
    UNKNOWN_MODE,
    debugMode = false,
    "",
    skipSanityCheck = false,
    printModelCheckerInputs = false,
    modelCheckerTimeout = 10,
    printCFG = false,
    lessPreciseBound = false,
    generateSynthetic = 0,
    maxGroups = DEFAULT_MAX_GROUPS,
    modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
    relationalPredicates = false,
  )

  "Parsing counterexample paths" should "be correct" in {
    UAutomizerVerifierUnitTest.testCases.foreach({
      testCase =>
        // val verifier = new UAutomizerVerifier(CommandLineArguments.DEBUG_MODE_ARGUMENTS)
        val verifier = new UAutomizerVerifier(arguments)
        val result = verifier.verify(testCase.input.asInstanceOf[BrboProgram])
        assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"Test `${testCase.name}` failed"))
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
      BrboProgram("test01", function, None, None, PreDefinedFunctions.allFunctionsList)
    }
    val test01Expected = """VerifierResult(UNKNOWN_RESULT,None)"""

    val test02 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement5, statement6, assertionFalse)), Set())
      BrboProgram("test02", function, None, None, PreDefinedFunctions.allFunctionsList)
    }
    val test02Expected =
      """VerifierResult(FALSE_RESULT,Some(Path:
        |  int i = 0; [Function `main`]
        |  int R = 0; [Function `main`]
        |  Call function `assume` with `(n > 0)` [Function `main`]
        |  !(!(cond)) [Function `assume`]
        |  [Function Exit] [Function `assume`]
        |  Call function `ndBool` no arguments [Function `main`]
        |  Call function `ndInt` no arguments [Function `ndBool`]
        |  return __VERIFIER_nondet_int(); [Function `ndInt`]
        |  int x = ndInt(); [Function `ndBool`]
        |  Call function `assume` with `((x == 0) || (x == 1))` [Function `ndBool`]
        |  !(!(cond)) [Function `assume`]
        |  [Function Exit] [Function `assume`]
        |  return x; [Function `ndBool`]
        |  (i < n) [Function `main`]
        |  int e = 0; [Function `main`]
        |  (i < 1) [Function `main`]
        |  e = a; [Function `main`]
        |  R = R + e; [Function `main`]
        |  i = i + 1; [Function `main`]
        |  (i < n) [Function `main`]
        |  int e = 0; [Function `main`]
        |  !((i < 1)) [Function `main`]
        |  e = b; [Function `main`]
        |  R = R + e; [Function `main`]
        |  i = i + 1; [Function `main`]
        |  !((i < n)) [Function `main`]
        |  Call function `assert` with `(R <= a)` [Function `main`]
        |  !(cond) [Function `assert`]
        |  ERROR: __VERIFIER_error(); [Function `assert`]
        |  return; [Function `assert`]))""".stripMargin

    val test03 = {
      val n = Identifier("n", INT)
      val reset1 = Reset(2, GreaterThan(n, Number(0)))
      val R2 = reset1.resourceVariable
      val reset2 = Reset(2)
      val use1 = Use(Some(2), Number(1), GreaterThan(n, Number(1)))
      val use2 = Use(Some(2), Number(2))
      val assertion = PreDefinedFunctions.createAssert(LessThanOrEqualTo(R2, Number(1)))
      val function = BrboFunction("main", VOID, List(n), Block(List(reset1, use1, reset2, use2, assertion)), Set[Int](2))
      BrboProgram("test03", function, None, None, PreDefinedFunctions.allFunctionsList)
    }
    val test03Expected =
      """VerifierResult(FALSE_RESULT,Some(Path:
        |  int C2 = -1; [Function `main`]
        |  int R2 = 0; [Function `main`]
        |  int S2 = 0; [Function `main`]
        |  if (n > 0) reset R2 [Function `main`]
        |  if (n > 1) use R2 1 [Function `main`]
        |  if (true) reset R2 [Function `main`]
        |  if (true) use R2 2 [Function `main`]
        |  Call function `assert` with `(R2 <= 1)` [Function `main`]
        |  !(cond) [Function `assert`]
        |  ERROR: __VERIFIER_error(); [Function `assert`]
        |  return; [Function `assert`]))""".stripMargin

    List[TestCase](
      TestCase("Must be unknown", test01, test01Expected), // This must time out!
      TestCase("Must fail", test02, test02Expected),
      TestCase("Identify uses and resets", test03, test03Expected),
    )
  }
}