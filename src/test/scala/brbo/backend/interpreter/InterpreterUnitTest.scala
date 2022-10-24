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
  val readVariable =
    """class Test {
      |  void main(int x) {
      |    f(x);
      |  }
      |
      |  void f(int x) {
      |  }
      |}""".stripMargin

  val expressionTests: List[TestCase] = List(TestCase("Read variable", readVariable, """???"""))
}