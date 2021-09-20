package brbo.frontend

import brbo.TestCase
import org.scalatest.flatspec.AnyFlatSpec

class BasicProcessorUnitTest extends AnyFlatSpec {
  BasicProcessorUnitTest.tests.foreach({
    testCase =>
      val (className, code) = testCase.input.asInstanceOf[(String, String)]
      val result = BasicProcessor.getTargetMethod(className, code)
      println(result.program)
  })
}

object BasicProcessorUnitTest {
  val tests: List[TestCase] = {
    val test01 = """class Test01 {
                   |  void f(int n, int m) {
                   |    int R = 0;
                   |    int i = 0;
                   |    while (i < n) {
                   |      int j = 0;
                   |      while (j < m) {
                   |        j++;
                   |        R = R + 1;
                   |      }
                   |      i++;
                   |    }
                   |  }
                   |}""".stripMargin

    List(
      TestCase("Test 01", ("Test01", test01), """""")
    )
  }
}