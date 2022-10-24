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

  val expressionTests: List[TestCase] = List(TestCase("Expression test", expressionTest,
    """GoodState$
      |Value: Some(20)
      |Store: (x -> 10)
      |Trace: [Store: (x -> 10)] ==> [(x;, None) ==> Store: (x -> 10)] ==> [(10;, None) ==> Store: (x -> 10)]
      |       [((x + 10);, None) ==> Store: (x -> 10)] ==> [(5;, None) ==> Store: (x -> 10)] ==> [(((x + 10) - 5);, None) ==> Store: (x -> 10)]
      |       [Store: (x -> 15)] ==> [(x;, None) ==> Store: (x -> 15)] ==> [(return x;, None) ==> Store: (x -> 15)]
      |       [(true;, None) ==> Store: (x -> 10)] ==> [(!(true);, None) ==> Store: (x -> 10)] ==> [Store: (b -> false)]
      |       [(b;, None) ==> Store: (b -> false)] ==> [(return b;, None) ==> Store: (b -> false)] ==> [(10;, None) ==> Store: (x -> 10)]
      |       [(x;, None) ==> Store: (x -> 10)] ==> [((10 < x);, None) ==> Store: (x -> 10)] ==> [(!((10 < x));, None) ==> Store: (x -> 10)]
      |       [Store: (b -> true)] ==> [(b;, None) ==> Store: (b -> true)] ==> [(return b;, None) ==> Store: (b -> true)]
      |       [(10;, None) ==> Store: (x -> 10)] ==> [(x;, None) ==> Store: (x -> 10)] ==> [((10 < x);, None) ==> Store: (x -> 10)]
      |       [Store: (b -> false)] ==> [(b;, None) ==> Store: (b -> false)] ==> [(return b;, None) ==> Store: (b -> false)]
      |       [(10;, None) ==> Store: (x -> 10)] ==> [(x;, None) ==> Store: (x -> 10)] ==> [((10 == x);, None) ==> Store: (x -> 10)]
      |       [Store: (b -> true)] ==> [(b;, None) ==> Store: (b -> true)] ==> [(return b;, None) ==> Store: (b -> true)]
      |       [(true;, None) ==> Store: (x -> 10)] ==> [(false;, None) ==> Store: (x -> 10)] ==> [((true && false);, None) ==> Store: (x -> 10)]
      |       [Store: (b -> false)] ==> [(b;, None) ==> Store: (b -> false)] ==> [(return b;, None) ==> Store: (b -> false)]
      |       [(true;, None) ==> Store: (x -> 10)] ==> [(false;, None) ==> Store: (x -> 10)] ==> [((true || false);, None) ==> Store: (x -> 10)]
      |       [Store: (b -> true)] ==> [(b;, None) ==> Store: (b -> true)] ==> [(return b;, None) ==> Store: (b -> true)]
      |       [(x;, None) ==> Store: (x -> 10)] ==> [(2;, None) ==> Store: (x -> 10)] ==> [((x * 2);, None) ==> Store: (x -> 10)]
      |       [Store: (x -> 20)] ==> [(x;, None) ==> Store: (x -> 20)] ==> [(return x;, None) ==> Store: (x -> 20)]""".stripMargin))
}