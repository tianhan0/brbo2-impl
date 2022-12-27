package brbo.backend2.qfuzz

import brbo.TestCase
import brbo.common.string.StringCompare
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.scalatest.flatspec.AnyFlatSpec

class ProgramTransformerUnitTest extends AnyFlatSpec {
  "Transforming a program for QFuzz" should "be correct" in {
    ProgramTransformerUnitTest.transformationTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        val transformedProgram = ProgramTransformer.transform(targetProgram.program)
        StringCompare.ignoreWhitespaces(transformedProgram.printToQFuzzJava(indent = 0), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object ProgramTransformerUnitTest {
  private val test01 =
    s"""class Test {
       |  void ${TargetProgram.MAIN_FUNCTION}(int n) {
       |    int R = 0;
       |    for (int i = 0; i < n; i++) {
       |      R = R + 5;
       |    }
       |  }
       |}""".stripMargin

  private val test02 =
    s"""abstract class Test {
       |  void ${TargetProgram.MAIN_FUNCTION}(int[] array) {
       |    int x = arrayLength(array);
       |    x = arraySum(array);
       |    x = arrayRead(array, 3);
       |  }
       |  abstract int arrayRead(int[] x, int index);
       |  abstract int arraySum(int[] x);
       |  abstract int arrayLength(int[] x);
       |}""".stripMargin

  private val transformationTests = List(
    TestCase("Test 01", test01,
      """class Test {
        |  void execute(int n, int INDEX_VARIABLE)
        |  {
        |    if (8 < n)
        |    {
        |      return;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    int R = 0;
        |    {
        |      int i = 0;
        |      while (i < n)
        |      {
        |        {
        |          {
        |            use(5);
        |            INDEX_VARIABLE = INDEX_VARIABLE - 1;
        |            if (INDEX_VARIABLE == 0)
        |            {
        |              return;
        |            }
        |            else
        |            {
        |              ;
        |            }
        |          }
        |        }
        |        i = i + 1;
        |      }
        |    }
        |  }
        |  int arrayRead(int[] array, int index) { return array[index]; }
        |  int arrayLength(int[] array) { return array.length; }
        |  int arraySum(int[] array) {
        |    int sum = 0;
        |    for (int i = 0; i < array.length; i++) {
        |      sum += array[i];
        |    }
        |    return sum;
        |  }
        |  void mostPreciseBound(boolean assertion) {}
        |  void lessPreciseBound(boolean assertion) {}
        |  void use(int n)
        |  {
        |    int i = 0;
        |    while (i < (n * 10))
        |    {
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin),
    TestCase("Test 02", test02,
      """class Test {
        |  void execute(int[] array, int INDEX_VARIABLE)
        |  {
        |    int x = arrayLength(array);
        |    x = arraySum(array);
        |    x = arrayRead(array, 3);
        |  }
        |  int arrayRead(int[] array, int index) { return array[index]; }
        |  int arrayLength(int[] array) { return array.length; }
        |  int arraySum(int[] array) {
        |    int sum = 0;
        |    for (int i = 0; i < array.length; i++) {
        |      sum += array[i];
        |    }
        |    return sum;
        |  }
        |  void mostPreciseBound(boolean assertion) {}
        |  void lessPreciseBound(boolean assertion) {}
        |  void use(int n)
        |  {
        |    int i = 0;
        |    while (i < (n * 10))
        |    {
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin),
  )
}