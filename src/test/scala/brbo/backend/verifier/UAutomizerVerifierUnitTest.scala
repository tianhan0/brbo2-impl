package brbo.backend.verifier

import brbo.common.CommandLineArguments
import brbo.common.TypeUtils.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.{StringCompare, TestCase}
import org.scalatest.flatspec.AnyFlatSpec

class UAutomizerVerifierUnitTest extends AnyFlatSpec {
  UAutomizerVerifierUnitTest.testCases.foreach({
    testCase =>
      val verifier = new UAutomizerVerifier(CommandLineArguments.DEFAULT_ARGUMENTS)
      val result = verifier.verify(testCase.input.asInstanceOf[BrboProgram])
      assert(StringCompare.ignoreWhitespaces(result.toString, testCase.expectedOutput, s"Test `${testCase.name}` failed"))
  })
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
    val statement3 = PreDefinedBrboFunctions.createAssume(GreaterThan(n, Number(0)))
    val statement5 = FunctionCall(None, FunctionCallExpr("ndBool", Nil, INT)) // To test parsing counterexample paths when involving function calls
    val statement6 = {
      val e = Identifier("e", INT)
      val statement1 = VariableDeclaration(e, Number(0))
      val statement2 = ITE(LessThan(i, Number(1)), Assignment(e, a), Assignment(e, b))
      val statement3 = Assignment(R, Addition(R, e))
      val statement4 = Assignment(i, Addition(i, Number(1)))
      Loop(LessThan(i, n), Block(List(statement1, statement2, statement3, statement4)))
    }
    val assertionTrue = PreDefinedBrboFunctions.createAssert(Or(LessThanOrEqualTo(R, a), LessThanOrEqualTo(R, Addition(a, Multiplication(Subtraction(n, Number(1)), b)))))
    val assertionFalse = PreDefinedBrboFunctions.createAssert(LessThanOrEqualTo(R, a))

    val test01 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement5, statement6, assertionTrue)))
      BrboProgram("test01", function, PreDefinedBrboFunctions.allFunctions)
    }
    val test01Expected = """VerifierResult(UNKNOWN,None)"""

    val test02 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement5, statement6, assertionFalse)))
      BrboProgram("test02", function, PreDefinedBrboFunctions.allFunctions)
    }
    val test02Expected = """VerifierResult(FALSE,Some(Path(List(int i = 0; [Function `main`], int R = 0; [Function `main`], !(cond) [Function `assume`], return __VERIFIER_nondet_int(); [Function `ndInt`], int x = ndInt(); [Function `ndBool`], !(cond) [Function `assume`], return x; [Function `ndBool`], (i < n) [Function `main`], int e = 0; [Function `main`], (i < 1) [Function `main`], e = a; [Function `main`], R = R + e; [Function `main`], i = i + 1; [Function `main`], (i < n) [Function `main`], int e = 0; [Function `main`], (i < 1) [Function `main`], e = b; [Function `main`], R = R + e; [Function `main`], i = i + 1; [Function `main`], (i < n) [Function `main`], !(cond) [Function `assert`], ERROR: __VERIFIER_error(); [Function `assert`]))))"""

    List[TestCase](
      TestCase("Must be unknown", test01, test01Expected),
      TestCase("Must fail", test02, test02Expected),
    )
  }
}