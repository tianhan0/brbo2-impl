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
        val interpreter = new Interpreter(targetProgram.program, debugMode = false)
        val exitState = interpreter.execute(List(Number(10)))
        StringCompare.ignoreWhitespaces(Interpreter.printState(exitState), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Interpreting commands" should "be correct" in {
    InterpreterUnitTest.commandTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        val interpreter = new Interpreter(targetProgram.program, debugMode = true)
        val exitState = interpreter.execute(List(Number(10)))
        StringCompare.ignoreWhitespaces(Interpreter.printState(exitState), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object InterpreterUnitTest {
  private val arithmeticTest =
    """class Test {
      |  void main(int x) {
      |    f(x + 10);
      |    f(x - 5);
      |    f(x * 2);
      |  }
      |
      |  int f(int x) { return x; }
      |}""".stripMargin

  private val negationTest =
    """class Test {
      |  void main(int x) {
      |    g(!true);
      |    g(!false);
      |  }
      |
      |  boolean g(boolean b) { return b; }
      |}""".stripMargin

  private val greaterThanTest =
    """class Test {
      |  void main(int x) {
      |    g(10 > x);
      |    g(x > 10);
      |  }
      |
      |  boolean g(boolean b) { return b; }
      |}""".stripMargin

  private val lessThanTest =
    """class Test {
      |  void main(int x) {
      |    g(10 < x);
      |    g(x < 10);
      |  }
      |
      |  boolean g(boolean b) { return b; }
      |}""".stripMargin

  private val equalTest =
    """class Test {
      |  void main(int x) {
      |    g(10 == x);
      |    g(true == false);
      |  }
      |
      |  boolean g(boolean b) { return b; }
      |}""".stripMargin

  private val andTest =
    """class Test {
      |  void main(int x) {
      |    g(true && false);
      |    g(true && true);
      |  }
      |
      |  boolean g(boolean b) { return b; }
      |}""".stripMargin

  private val orTest =
    """class Test {
      |  void main(int x) {
      |    g(true || false);
      |    g(false || false);
      |  }
      |
      |  boolean g(boolean b) { return b; }
      |}""".stripMargin

  val expressionTests: List[TestCase] = List(
    TestCase("ArithmeticTest", arithmeticTest,
      """GoodState$
        |Value: Some(20)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [(x + 10) ==> Store: (x -> 10)]
        |       [Store: (x -> 20)]
        |       [x ==> Store: (x -> 20)]
        |       [return x; ==> Store: (x -> 20)]
        |       [x ==> Store: (x -> 10)]
        |       [5 ==> Store: (x -> 10)]
        |       [(x - 5) ==> Store: (x -> 10)]
        |       [Store: (x -> 5)]
        |       [x ==> Store: (x -> 5)]
        |       [return x; ==> Store: (x -> 5)]
        |       [x ==> Store: (x -> 10)]
        |       [2 ==> Store: (x -> 10)]
        |       [(x * 2) ==> Store: (x -> 10)]
        |       [Store: (x -> 20)]
        |       [x ==> Store: (x -> 20)]
        |       [return x; ==> Store: (x -> 20)]""".stripMargin),
    TestCase("negationTest", negationTest,
      """GoodState$
        |Value: Some(true)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [true ==> Store: (x -> 10)]
        |       [!(true) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]
        |       [false ==> Store: (x -> 10)]
        |       [!(false) ==> Store: (x -> 10)]
        |       [Store: (b -> true)]
        |       [b ==> Store: (b -> true)]
        |       [return b; ==> Store: (b -> true)]""".stripMargin),
    TestCase("greaterThanTest", greaterThanTest,
      """GoodState$
        |Value: Some(false)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [(10 < x) ==> Store: (x -> 10)]
        |       [!((10 < x)) ==> Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [(10 == x) ==> Store: (x -> 10)]
        |       [!((10 == x)) ==> Store: (x -> 10)]
        |       [(!((10 < x)) && !((10 == x))) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]
        |       [x ==> Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [(x < 10) ==> Store: (x -> 10)]
        |       [!((x < 10)) ==> Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [(x == 10) ==> Store: (x -> 10)]
        |       [!((x == 10)) ==> Store: (x -> 10)]
        |       [(!((x < 10)) && !((x == 10))) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]""".stripMargin),
    TestCase("lessThanTest", lessThanTest,
      """GoodState$
        |Value: Some(false)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [(10 < x) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]
        |       [x ==> Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [(x < 10) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]""".stripMargin),
    TestCase("equalTest", equalTest,
      """GoodState$
        |Value: Some(false)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [10 ==> Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [(10 == x) ==> Store: (x -> 10)]
        |       [Store: (b -> true)]
        |       [b ==> Store: (b -> true)]
        |       [return b; ==> Store: (b -> true)]
        |       [true ==> Store: (x -> 10)]
        |       [false ==> Store: (x -> 10)]
        |       [(true == false) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]""".stripMargin),
    TestCase("andTest", andTest,
      """GoodState$
        |Value: Some(true)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [true ==> Store: (x -> 10)]
        |       [false ==> Store: (x -> 10)]
        |       [(true && false) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]
        |       [true ==> Store: (x -> 10)]
        |       [true ==> Store: (x -> 10)]
        |       [(true && true) ==> Store: (x -> 10)]
        |       [Store: (b -> true)]
        |       [b ==> Store: (b -> true)]
        |       [return b; ==> Store: (b -> true)]""".stripMargin),
    TestCase("orTest", orTest,
      """GoodState$
        |Value: Some(false)
        |Store: (x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [true ==> Store: (x -> 10)]
        |       [false ==> Store: (x -> 10)]
        |       [(true || false) ==> Store: (x -> 10)]
        |       [Store: (b -> true)]
        |       [b ==> Store: (b -> true)]
        |       [return b; ==> Store: (b -> true)]
        |       [false ==> Store: (x -> 10)]
        |       [false ==> Store: (x -> 10)]
        |       [(false || false) ==> Store: (x -> 10)]
        |       [Store: (b -> false)]
        |       [b ==> Store: (b -> false)]
        |       [return b; ==> Store: (b -> false)]""".stripMargin)
  )

  private val assignmentTest =
    """class Test {
      |  void main(int x) {
      |    int y = x;
      |    y = y + 100;
      |  }
      |}""".stripMargin

  private val loopBreakTest =
    """class Test {
      |  void main(int x) {
      |    int i = 0;
      |    while (i < 10) {
      |      if (i == 2)
      |        break;
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  private val loopContinueTest =
    """class Test {
      |  void main(int x) {
      |    int i = 0;
      |    while (i < 3) {
      |      i++;
      |      if (x == 10) {
      |        continue;
      |      }
      |      i = 0;
      |    }
      |  }
      |}""".stripMargin

  private val blockTest =
    """class Test {
      |  void main(int x) {
      |    int a = x;
      |    int b = a + 10;
      |    int c = b + 100;
      |  }
      |}""".stripMargin

  private val iteTest =
    """class Test {
      |  void main(int x) {
      |    int a = 0;
      |    if (x > 10) {
      |      a = a + 100;
      |    }
      |    else {
      |      a = a + 1000;
      |    }
      |  }
      |}""".stripMargin

  private val loopTest =
    """class Test {
      |  void main(int x) {
      |    int i = 0;
      |    while (i < 4) {
      |      i++;
      |    }
      |  }
      |}""".stripMargin

  private val returnTest =
    """class Test {
      |  void main(int x) {
      |    int i = x;
      |    i++;
      |  }
      |
      |  int f(int x) { return x; }
      |}""".stripMargin

  private val useTest =
    """class Test {
      |  void main(int x) {
      |    use(1, 10, x > 10);
      |    use(1, 100, x >= 10);
      |  }
      |
      |  void use(int x, int cost, boolean condition) {}
      |}""".stripMargin

  private val resetTest =
    """class Test {
      |  void main(int x) {
      |    use(1, 100);
      |    reset(1, x >= 10);
      |    use(1, 200);
      |    reset(1, x > 10);
      |  }
      |
      |  void use(int x, int cost, boolean condition) {}
      |  void use(int x, int cost) {}
      |  void reset(int x, boolean condition) {}
      |}""".stripMargin

  val commandTests: List[TestCase] = List(
    TestCase("assignmentTest", assignmentTest,
      """GoodState$
        |Value: None
        |Store: (x -> 10, y -> 110)
        |Trace: [Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [int y = x; ==> Store: (x -> 10, y -> 10)]
        |       [y ==> Store: (x -> 10, y -> 10)]
        |       [100 ==> Store: (x -> 10, y -> 10)]
        |       [(y + 100) ==> Store: (x -> 10, y -> 10)]
        |       [y = y + 100; ==> Store: (x -> 10, y -> 110)]""".stripMargin),
    TestCase("loopBreakTest", loopBreakTest,
      """JumpState$
        |Store: (i -> 2, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [0 ==> Store: (x -> 10)]
        |       [int i = 0; ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [10 ==> Store: (i -> 0, x -> 10)]
        |       [(i < 10) ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [2 ==> Store: (i -> 0, x -> 10)]
        |       [(i == 2) ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [1 ==> Store: (i -> 0, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 0, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [10 ==> Store: (i -> 1, x -> 10)]
        |       [(i < 10) ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [2 ==> Store: (i -> 1, x -> 10)]
        |       [(i == 2) ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [1 ==> Store: (i -> 1, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 1, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 2, x -> 10)]
        |       [i ==> Store: (i -> 2, x -> 10)]
        |       [10 ==> Store: (i -> 2, x -> 10)]
        |       [(i < 10) ==> Store: (i -> 2, x -> 10)]
        |       [i ==> Store: (i -> 2, x -> 10)]
        |       [2 ==> Store: (i -> 2, x -> 10)]
        |       [(i == 2) ==> Store: (i -> 2, x -> 10)]
        |       [break; ==> Store: (i -> 2, x -> 10)]""".stripMargin),
    TestCase("loopContinueTest", loopContinueTest,
      """GoodState$
        |Value: None
        |Store: (i -> 3, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [0 ==> Store: (x -> 10)]
        |       [int i = 0; ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [3 ==> Store: (i -> 0, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [1 ==> Store: (i -> 0, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 0, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 1, x -> 10)]
        |       [x ==> Store: (i -> 1, x -> 10)]
        |       [10 ==> Store: (i -> 1, x -> 10)]
        |       [(x == 10) ==> Store: (i -> 1, x -> 10)]
        |       [continue; ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [3 ==> Store: (i -> 1, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [1 ==> Store: (i -> 1, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 1, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 2, x -> 10)]
        |       [x ==> Store: (i -> 2, x -> 10)]
        |       [10 ==> Store: (i -> 2, x -> 10)]
        |       [(x == 10) ==> Store: (i -> 2, x -> 10)]
        |       [continue; ==> Store: (i -> 2, x -> 10)]
        |       [i ==> Store: (i -> 2, x -> 10)]
        |       [3 ==> Store: (i -> 2, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 2, x -> 10)]
        |       [i ==> Store: (i -> 2, x -> 10)]
        |       [1 ==> Store: (i -> 2, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 2, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 3, x -> 10)]
        |       [x ==> Store: (i -> 3, x -> 10)]
        |       [10 ==> Store: (i -> 3, x -> 10)]
        |       [(x == 10) ==> Store: (i -> 3, x -> 10)]
        |       [continue; ==> Store: (i -> 3, x -> 10)]
        |       [i ==> Store: (i -> 3, x -> 10)]
        |       [3 ==> Store: (i -> 3, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 3, x -> 10)]
        |       [i ==> Store: (i -> 3, x -> 10)]
        |       [3 ==> Store: (i -> 3, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 3, x -> 10)]
        |       [i ==> Store: (i -> 3, x -> 10)]
        |       [3 ==> Store: (i -> 3, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 3, x -> 10)]
        |       [i ==> Store: (i -> 3, x -> 10)]
        |       [3 ==> Store: (i -> 3, x -> 10)]
        |       [(i < 3) ==> Store: (i -> 3, x -> 10)]""".stripMargin),
    TestCase("blockTest", blockTest,
      """GoodState$
        |Value: None
        |Store: (a -> 10, b -> 20, c -> 120, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [int a = x; ==> Store: (a -> 10, x -> 10)]
        |       [a ==> Store: (a -> 10, x -> 10)]
        |       [10 ==> Store: (a -> 10, x -> 10)]
        |       [(a + 10) ==> Store: (a -> 10, x -> 10)]
        |       [int b = a + 10; ==> Store: (a -> 10, b -> 20, x -> 10)]
        |       [b ==> Store: (a -> 10, b -> 20, x -> 10)]
        |       [100 ==> Store: (a -> 10, b -> 20, x -> 10)]
        |       [(b + 100) ==> Store: (a -> 10, b -> 20, x -> 10)]
        |       [int c = b + 100; ==> Store: (a -> 10, b -> 20, c -> 120, x -> 10)]""".stripMargin),
    TestCase("iteTest", iteTest,
      """GoodState$
        |Value: None
        |Store: (a -> 1000, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [0 ==> Store: (x -> 10)]
        |       [int a = 0; ==> Store: (a -> 0, x -> 10)]
        |       [x ==> Store: (a -> 0, x -> 10)]
        |       [10 ==> Store: (a -> 0, x -> 10)]
        |       [(x < 10) ==> Store: (a -> 0, x -> 10)]
        |       [!((x < 10)) ==> Store: (a -> 0, x -> 10)]
        |       [x ==> Store: (a -> 0, x -> 10)]
        |       [10 ==> Store: (a -> 0, x -> 10)]
        |       [(x == 10) ==> Store: (a -> 0, x -> 10)]
        |       [!((x == 10)) ==> Store: (a -> 0, x -> 10)]
        |       [(!((x < 10)) && !((x == 10))) ==> Store: (a -> 0, x -> 10)]
        |       [a ==> Store: (a -> 0, x -> 10)]
        |       [1000 ==> Store: (a -> 0, x -> 10)]
        |       [(a + 1000) ==> Store: (a -> 0, x -> 10)]
        |       [a = a + 1000; ==> Store: (a -> 1000, x -> 10)]""".stripMargin),
    TestCase("loopTest", loopTest,
      """GoodState$
        |Value: None
        |Store: (i -> 4, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [0 ==> Store: (x -> 10)]
        |       [int i = 0; ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [4 ==> Store: (i -> 0, x -> 10)]
        |       [(i < 4) ==> Store: (i -> 0, x -> 10)]
        |       [i ==> Store: (i -> 0, x -> 10)]
        |       [1 ==> Store: (i -> 0, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 0, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [4 ==> Store: (i -> 1, x -> 10)]
        |       [(i < 4) ==> Store: (i -> 1, x -> 10)]
        |       [i ==> Store: (i -> 1, x -> 10)]
        |       [1 ==> Store: (i -> 1, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 1, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 2, x -> 10)]
        |       [i ==> Store: (i -> 2, x -> 10)]
        |       [4 ==> Store: (i -> 2, x -> 10)]
        |       [(i < 4) ==> Store: (i -> 2, x -> 10)]
        |       [i ==> Store: (i -> 2, x -> 10)]
        |       [1 ==> Store: (i -> 2, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 2, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 3, x -> 10)]
        |       [i ==> Store: (i -> 3, x -> 10)]
        |       [4 ==> Store: (i -> 3, x -> 10)]
        |       [(i < 4) ==> Store: (i -> 3, x -> 10)]
        |       [i ==> Store: (i -> 3, x -> 10)]
        |       [1 ==> Store: (i -> 3, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 3, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 4, x -> 10)]
        |       [i ==> Store: (i -> 4, x -> 10)]
        |       [4 ==> Store: (i -> 4, x -> 10)]
        |       [(i < 4) ==> Store: (i -> 4, x -> 10)]""".stripMargin),
    TestCase("returnTest", returnTest,
      """GoodState$
        |Value: None
        |Store: (i -> 11, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [x ==> Store: (x -> 10)]
        |       [int i = x; ==> Store: (i -> 10, x -> 10)]
        |       [i ==> Store: (i -> 10, x -> 10)]
        |       [1 ==> Store: (i -> 10, x -> 10)]
        |       [(i + 1) ==> Store: (i -> 10, x -> 10)]
        |       [i = i + 1; ==> Store: (i -> 11, x -> 10)]""".stripMargin),
    TestCase("useTest", useTest,
      """GoodState$
        |Value: None
        |Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [0 ==> Store: (x -> 10)]
        |       [int C1 = 0; ==> Store: (C1 -> 0, x -> 10)]
        |       [0 ==> Store: (C1 -> 0, x -> 10)]
        |       [int R1 = 0; ==> Store: (C1 -> 0, R1 -> 0, x -> 10)]
        |       [-2147483648 ==> Store: (C1 -> 0, R1 -> 0, x -> 10)]
        |       [int S1 = -2147483648; ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [x ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [10 ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [(x < 10) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [!((x < 10)) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [x ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [10 ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [(x == 10) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [!((x == 10)) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [(!((x < 10)) && !((x == 10))) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [(!((x < 10)) && !((x == 10))) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [x ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [10 ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [(x < 10) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [!((x < 10)) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [!((x < 10)) ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [100 ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [if (!((x < 10))) use R1 100 <cost=100> ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]""".stripMargin),
    TestCase("resetTest", resetTest,
      """GoodState$
        |Value: None
        |Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)
        |Trace: [Store: (x -> 10)]
        |       [0 ==> Store: (x -> 10)]
        |       [int C1 = 0; ==> Store: (C1 -> 0, x -> 10)]
        |       [0 ==> Store: (C1 -> 0, x -> 10)]
        |       [int R1 = 0; ==> Store: (C1 -> 0, R1 -> 0, x -> 10)]
        |       [-2147483648 ==> Store: (C1 -> 0, R1 -> 0, x -> 10)]
        |       [int S1 = -2147483648; ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [true ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [true ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [100 ==> Store: (C1 -> 0, R1 -> 0, S1 -> -2147483648, x -> 10)]
        |       [if (true) use R1 100 <cost=100> ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [x ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [10 ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [(x < 10) ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [!((x < 10)) ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [!((x < 10)) ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [if (!((x < 10))) reset R1 ==> Store: (C1 -> 0, R1 -> 100, S1 -> -2147483648, x -> 10)]
        |       [true ==> Store: (C1 -> 1, R1 -> 0, S1 -> 100, x -> 10)]
        |       [true ==> Store: (C1 -> 1, R1 -> 0, S1 -> 100, x -> 10)]
        |       [200 ==> Store: (C1 -> 1, R1 -> 0, S1 -> 100, x -> 10)]
        |       [if (true) use R1 200 <cost=200> ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [x ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [10 ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [(x < 10) ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [!((x < 10)) ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [x ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [10 ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [(x == 10) ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [!((x == 10)) ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [(!((x < 10)) && !((x == 10))) ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]
        |       [(!((x < 10)) && !((x == 10))) ==> Store: (C1 -> 1, R1 -> 200, S1 -> 100, x -> 10)]""".stripMargin),
  )
}