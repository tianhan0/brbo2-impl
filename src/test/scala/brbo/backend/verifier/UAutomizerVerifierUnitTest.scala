package brbo.backend.verifier

import brbo.TestCase
import brbo.common.TypeUtils.BrboType.{INT, VOID}
import brbo.common.ast._
import org.scalatest.flatspec.AnyFlatSpec

class UAutomizerVerifierUnitTest extends AnyFlatSpec {
  UAutomizerVerifierUnitTest.testCases.foreach({
    testCase =>
      val verifier = new UAutomizerVerifier
      verifier.verify(testCase.input.asInstanceOf[BrboProgram])
  })
}

object UAutomizerVerifierUnitTest {
  val testCases: List[TestCase] = {
    val i = Identifier("i", INT)
    val n = Identifier("n", INT)
    val a = Identifier("a", INT)
    val b = Identifier("b", INT)
    val R = Identifier("R", INT)
    val statement1 = VariableDeclaration("i", INT, Number(0))
    val statement2 = VariableDeclaration("R", INT, Number(0))
    val statement3 = Assume(GreaterThan(n, Number(0)))
    val statement4 = {
      val e = Identifier("e", INT)
      val statement1 = VariableDeclaration("e", INT, Number(0))
      val statement2 = ITE(LessThan(i, Number(1)), Assignment(e, a), Assignment(e, b))
      val statement3 = Assignment(R, Addition(R, e))
      val statement4 = Assignment(i, Addition(i, Number(1)))
      Loop(LessThan(i, n), Block(List(statement1, statement2, statement3, statement4)))
    }
    val assertionTrue = Assert(Or(LessThanOrEqualTo(R, a), LessThanOrEqualTo(R, Addition(a, Multiplication(Subtraction(n, Number(1)), b)))))
    val assertionFalse = Assert(LessThanOrEqualTo(R, a))

    val test01 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement4, assertionTrue)))
      BrboProgram(function)
    }
    val test01Expected = """"""

    val test02 = {
      val function = BrboFunction("main", VOID, List(n, a, b), Block(List(statement1, statement2, statement3, statement4, assertionFalse)))
      BrboProgram(function)
    }
    val test02Expected = """"""

    List[TestCase](
      TestCase("Test 1", test01, test01Expected),
      TestCase("Test 2", test02, test02Expected),
    )
  }
}