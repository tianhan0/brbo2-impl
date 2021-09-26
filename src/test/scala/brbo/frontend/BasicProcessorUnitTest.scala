package brbo.frontend

import brbo.TestCase
import brbo.common.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class BasicProcessorUnitTest extends AnyFlatSpec {
  "Parsing Java source code" should "be correct" in {
    BasicProcessorUnitTest.tests.foreach({
      testCase =>
        val (className, code) = testCase.input.asInstanceOf[(String, String)]
        val result = BasicProcessor.getTargetProgram(className, code)
        assert(StringCompare.ignoreWhitespaces(result.program.mainFunction.toString, testCase.expectedOutput, s"`${testCase.name}` failed!"))
    })
  }
}

object BasicProcessorUnitTest {
  val tests: List[TestCase] = {
    val test01 =
      """class Test {
        |  void main(int n, int m) {
        |    int i = 0;
        |  }
        |}""".stripMargin
    val test02 =
      """class Test {
        |  int main(int n, int m) {
        |    return n;
        |  }
        |}""".stripMargin
    val test03 =
      """class Test {
        |  void main(int n, int m) {
        |    int z = n + m;
        |  }
        |}""".stripMargin
    val test04 =
      """class Test {
        |  void main(int n, int m) {
        |    if (n > m || n >= m || n == m) {
        |      int z = (((n + 1) - 2) * 3) / 4;
        |    }
        |    if (n < m && n <= m && n != m) {
        |      return;
        |    }
        |  }
        |}""".stripMargin
    val test05 =
      """class Test {
        |  void main(int n, int m) {
        |    int z = 0;
        |    z += n;
        |    z -= n;
        |    z *= n;
        |    z /= n;
        |  }
        |}""".stripMargin
    val test06 =
      """class Test {
        |  void main(int n, int m) {
        |    int z = -1;
        |    if (!true) {
        |      z += +1;
        |    }
        |    z++;
        |    ++z;
        |    z--;
        |    --z;
        |  }
        |}""".stripMargin
    val test07 =
      """class Test {
        |  void main(int n, int m) {
        |    int z = f(n);
        |  }
        |
        |  int f(int x) {
        |    return x;
        |  }
        |}""".stripMargin
    val test08 =
      """class Test {
        |  void main(int n, int m) {
        |    assume(n == m);
        |  }
        |
        |  void assume(boolean b) {
        |  }
        |}""".stripMargin
    val test09 =
      """class Test {
        |  void main(int n, int m) {
        |    int x = 0;
        |    int y = 0;
        |    int z = 0;
        |    ;
        |  }
        |}""".stripMargin
    val test10 =
      """class Test {
        |  void main(int n, int m) {
        |    for (int i = 0, k = 0; i < 10; i++, k = 1) {
        |      int j = 0;
        |      break;
        |    }
        |  }
        |}""".stripMargin
    val test11 =
      """class Test {
        |  void main(int n, int m) {
        |    int i = 0;
        |    while (i < n) {
        |      int j = 0;
        |      continue;
        |    }
        |  }
        |}""".stripMargin
    val test12 =
      """class Test {
        |  void main(int n, int m) {
        |    if (n > 0)
        |      return;
        |    else
        |      ;
        |  }
        |}""".stripMargin

    List(
      TestCase("Variable declaration", ("Test", test01),
        """void main(int n, int m)
          |{
          |  int i = 0;
          |}""".stripMargin),
      TestCase("Return", ("Test", test02),
        """int main(int n, int m)
          |{
          |  return n;
          |}""".stripMargin),
      TestCase("Assignment", ("Test", test03),
        """void main(int n, int m)
          |{
          |  int z = n + m;
          |}""".stripMargin),
      TestCase("Binary expressions", ("Test", test04),
        """void main(int n, int m)
          |{
          |  if (((n > m) || (n >= m)) || (n == m))
          |  {
          |    int z = (((n + 1) - 2) * 3) / 4;
          |  }
          |  else
          |  {
          |
          |  }
          |  if (((n < m) && (n <= m)) && (n != m))
          |  {
          |    return;
          |  }
          |  else
          |  {
          |
          |  }
          |}""".stripMargin),
      TestCase("Compound assignments", ("Test", test05),
        """void main(int n, int m)
          |{
          |  int z = 0;
          |  z = z + n;
          |  z = z - n;
          |  z = z * n;
          |  z = z / n;
          |}""".stripMargin),
      TestCase("Unary", ("Test", test06),
        """void main(int n, int m)
          |{
          |  int z = -1;
          |  if (!(true))
          |  {
          |    z = z + 1;
          |  }
          |  else
          |  {
          |
          |  }
          |  z = z + 1;
          |  z = z + 1;
          |  z = z - 1;
          |  z = z - 1;
          |}""".stripMargin),
      TestCase("Method invocation", ("Test", test07),
        """void main(int n, int m)
          |{
          |  int z = f(n);
          |}""".stripMargin),
      TestCase("Method invocation (predefined function)", ("Test", test08),
        """void main(int n, int m)
          |{
          |  assume(n == m);
          |}""".stripMargin),
      TestCase("Block and Skip", ("Test", test09),
        """void main(int n, int m)
          |{
          |  int x = 0;
          |  int y = 0;
          |  int z = 0;
          |  ;
          |}""".stripMargin),
      TestCase("For and Break", ("Test", test10), """void main(int n, int m)
                                                    |{
                                                    |  {
                                                    |    int i = 0;
                                                    |    int k = 0;
                                                    |    while (i < 10)
                                                    |    {
                                                    |      {
                                                    |        int j = 0;
                                                    |        break;
                                                    |      }
                                                    |      i = i + 1;
                                                    |      k = 1;
                                                    |    }
                                                    |  }
                                                    |}""".stripMargin),
      TestCase("While and Continue", ("Test", test11), """void main(int n, int m)
                                                         |{
                                                         |  int i = 0;
                                                         |  while (i < n)
                                                         |  {
                                                         |    int j = 0;
                                                         |    continue;
                                                         |  }
                                                         |}""".stripMargin),
      TestCase("ITE", ("Test", test12), """void main(int n, int m)
                                          |{
                                          |  if (n > 0)
                                          |    return;
                                          |  else
                                          |    ;
                                          |}""".stripMargin),
    )
  }
}