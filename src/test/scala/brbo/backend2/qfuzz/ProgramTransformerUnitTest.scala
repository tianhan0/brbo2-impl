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
       |    if (n < 5) {
       |      return;
       |    }
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
        |  int execute(int n, int INDEX_VARIABLE)
        |  {
        |    int USE_COUNT = 0;
        |    if (8 < n)
        |    {
        |      return USE_COUNT;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    if (n < 5)
        |    {
        |      return USE_COUNT;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      use(5);
        |      USE_COUNT = USE_COUNT + 1;
        |      INDEX_VARIABLE = INDEX_VARIABLE - 1;
        |      if (INDEX_VARIABLE == 0)
        |      {
        |        return USE_COUNT;
        |      }
        |      else
        |      {
        |        ;
        |      }
        |      i = i + 1;
        |    }
        |    return USE_COUNT;
        |  }
        |  int arrayRead(int[] array, int index) { return array[index]; }
        |  int arrayLength(int[] array) { return array.length; }
        |  int arraySum(int[] array) {
        |    int sum = 0;
        |    for (int element : array) {
        |      sum += element;
        |    }
        |    return sum;
        |  }
        |  void mostPreciseBound(boolean assertion) {}
        |  void lessPreciseBound(boolean assertion) {}
        |  boolean ndBool2(int... values) {
        |    int sum = 0;
        |    for (int value : values) {
        |      sum += value;
        |    }
        |    // mod 2 results in a higher chance of producing an alternative value, when compared with mod 3
        |    return sum % 2 == 0;
        |  }
        |  int ndInt2(int lower, int upper) {
        |    if (upper < lower)
        |      System.exit(-1);
        |    return upper > lower ? lower + 1 : upper;
        |  }
        |  void use(int n)
        |  {
        |    int i = 0;
        |    while (i < (n * 1000))
        |    {
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin),
    TestCase("Test 02", test02,
      """class Test {
        |  int execute(int[] array, int INDEX_VARIABLE)
        |  {
        |    int USE_COUNT = 0;
        |    int x = arrayLength(array);
        |    x = arraySum(array);
        |    x = arrayRead(array, 3);
        |    return USE_COUNT;
        |  }
        |  int arrayRead(int[] array, int index) { return array[index]; }
        |  int arrayLength(int[] array) { return array.length; }
        |  int arraySum(int[] array) {
        |    int sum = 0;
        |    for (int element : array) {
        |      sum += element;
        |    }
        |    return sum;
        |  }
        |  void mostPreciseBound(boolean assertion) {}
        |  void lessPreciseBound(boolean assertion) {}
        |  boolean ndBool2(int... values) {
        |    int sum = 0;
        |    for (int value : values) {
        |      sum += value;
        |    }
        |    // mod 2 results in a higher chance of producing an alternative value, when compared with mod 3
        |    return sum % 2 == 0;
        |  }
        |  int ndInt2(int lower, int upper) {
        |    if (upper < lower)
        |      System.exit(-1);
        |    return upper > lower ? lower + 1 : upper;
        |  }
        |  void use(int n)
        |  {
        |    int i = 0;
        |    while (i < (n * 1000))
        |    {
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin),
  )
}