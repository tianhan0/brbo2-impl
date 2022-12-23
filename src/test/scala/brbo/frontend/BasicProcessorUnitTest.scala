package brbo.frontend

import brbo.TestCase
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class BasicProcessorUnitTest extends AnyFlatSpec {
  "Parsing Java source code" should "be correct" in {
    BasicProcessorUnitTest.tests.foreach({
      testCase =>
        val (className, code) = testCase.input.asInstanceOf[(String, String)]
        val result = BasicProcessor.getTargetProgram(className, code)
        StringCompare.ignoreWhitespaces(result.program.mainFunction.printToC(0), testCase.expectedOutput, s"`${testCase.name}` failed!")
    })
  }
}

object BasicProcessorUnitTest {
  val tests: List[TestCase] = {
    val test01 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    int i = 0;
        |  }
        |}""".stripMargin
    val test02 =
      s"""class Test {
        |  int ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    return n;
        |  }
        |}""".stripMargin
    val test03 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    int z = n + m;
        |  }
        |}""".stripMargin
    val test04 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    if (n > m || n >= m || n == m) {
        |      int z = (((n + 1) - 2) * 3);
        |    }
        |    if (n < m && n <= m && n != m) {
        |      return;
        |    }
        |  }
        |}""".stripMargin
    val test05 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    int z = 0;
        |    z += n;
        |    z -= n;
        |    z *= n;
        |  }
        |}""".stripMargin
    val test06 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
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
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    int z = f(n);
        |  }
        |
        |  int f(int x) {
        |    return x;
        |  }
        |}""".stripMargin
    val test08 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    assume2(n == m);
        |  }
        |
        |  void assume2(boolean b) {
        |  }
        |}""".stripMargin
    val test09 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    int x = 0;
        |    int y = 0;
        |    int z = 0;
        |    ;
        |  }
        |}""".stripMargin
    val test10 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    for (int i = 0, k = 0; i < 10; i++, k = 1) {
        |      int j = 0;
        |      break;
        |    }
        |  }
        |}""".stripMargin
    val test11 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    int i = 0;
        |    while (i < n) {
        |      int j = 0;
        |      continue;
        |    }
        |  }
        |}""".stripMargin
    val test12 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(int n, int m) {
        |    if (n > 0)
        |      return;
        |    else
        |      ;
        |  }
        |}""".stripMargin
    val test13 =
      s"""class Test {
        |  void ${TargetProgram.MAIN_FUNCTION}(String arg) {
        |    String x = "helloworld";
        |  }
        |}""".stripMargin

    List(
      TestCase("Variable declaration", ("Test", test01),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  int i = 0;
          |}""".stripMargin),
      TestCase("Return", ("Test", test02),
        s"""int ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  return n;
          |}""".stripMargin),
      TestCase("Assignment", ("Test", test03),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  int z = n + m;
          |}""".stripMargin),
      TestCase("Binary expressions", ("Test", test04),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  if (((!((n < m)) && !((n == m))) || !((n < m))) || (n == m))
          |  {
          |    int z = ((n + 1) - 2) * 3;
          |  }
          |  else
          |  {
          |
          |  }
          |  if (((n < m) && ((n < m) || (n == m))) && !((n == m)))
          |  {
          |    return;
          |  }
          |  else
          |  {
          |
          |  }
          |}""".stripMargin),
      TestCase("Compound assignments", ("Test", test05),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  int z = 0;
          |  z = z + n;
          |  z = z - n;
          |  z = z * n;
          |}""".stripMargin),
      TestCase("Unary", ("Test", test06),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
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
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  int z = f(n);
          |}""".stripMargin),
      TestCase("Method invocation (predefined function)", ("Test", test08),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  assume2(n == m);
          |}""".stripMargin),
      TestCase("Block and Skip", ("Test", test09),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  int x = 0;
          |  int y = 0;
          |  int z = 0;
          |  ;
          |}""".stripMargin),
      TestCase("For and Break", ("Test", test10),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
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
      TestCase("While and Continue", ("Test", test11),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  int i = 0;
          |  while (i < n)
          |  {
          |    int j = 0;
          |    continue;
          |  }
          |}""".stripMargin),
      TestCase("ITE", ("Test", test12),
        s"""void ${TargetProgram.MAIN_FUNCTION}(int n, int m)
          |{
          |  if (!((n < 0)) && !((n == 0)))
          |  {
          |    return;
          |  }
          |  else
          |  {
          |    ;
          |  }
          |}""".stripMargin),
      TestCase("StringLiteral", ("Test", test13),
        s"""void ${TargetProgram.MAIN_FUNCTION}""" + """($string$ arg)
          |{
          |  $string$ x = helloworld;
          |}""".stripMargin),
    )
  }
}