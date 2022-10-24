package brbo.backend.interpreter

import brbo.TestCase
import brbo.common.ast._
import brbo.common.string.StringCompare
import brbo.frontend.BasicProcessor
import org.scalatest.flatspec.AnyFlatSpec

class InterpreterUnitTest extends AnyFlatSpec {
  "Interpreting expressions" should "be correct" in {
    InterpreterUnitTest.expressionTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        val interpreter = new Interpreter(targetProgram.program, debugMode = true)
        val exitState = interpreter.execute(List(Number(10)))
        StringCompare.ignoreWhitespaces(Interpreter.printState(exitState), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object InterpreterUnitTest {
  private val expressionTest =
    """class Test {
      |  void main(int x) {
      |    f(x + 10 - 5);
      |    g(!true);
      |    g(10 > x);
      |    g(10 < x);
      |    g(10 == x);
      |    g(true && false);
      |    g(true || false);
      |    f(x * 2);
      |  }
      |
      |  int f(int x) { return x; }
      |
      |  boolean g(boolean b) { return b;}
      |}""".stripMargin

  val expressionTests: List[TestCase] = List(TestCase("Expression test", expressionTest, """???"""))
}