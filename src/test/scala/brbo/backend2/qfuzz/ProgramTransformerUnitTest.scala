package brbo.backend2.qfuzz

import brbo.TestCase
import brbo.backend2.qfuzz.ProgramTransformerUnitTest.loopIterationMultiplier
import brbo.common.ast.PrintStyle.QFuzzJavaStyle
import brbo.common.string.StringCompare
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.scalatest.flatspec.AnyFlatSpec

class ProgramTransformerUnitTest extends AnyFlatSpec {
  "Transforming a program for modified QFuzz" should "be correct" in {
    ProgramTransformerUnitTest.transformationTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        val transformedProgram = ProgramTransformer.transform(
          targetProgram.program,
          loopIterationMultiplier = loopIterationMultiplier,
          mode = DriverGenerator.Modified
        )
        StringCompare.ignoreWhitespaces(transformedProgram.print(indent = 0, style = QFuzzJavaStyle), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Transforming a program for naive QFuzz" should "be correct" in {
    ProgramTransformerUnitTest.naiveTransformationTests.foreach({
      testCase =>
        val targetProgram = BasicProcessor.getTargetProgram("Test", testCase.input.asInstanceOf[String])
        val transformedProgram = ProgramTransformer.transform(
          targetProgram.program,
          loopIterationMultiplier = loopIterationMultiplier,
          mode = DriverGenerator.Naive
        )
        StringCompare.ignoreWhitespaces(transformedProgram.print(indent = 0, style = QFuzzJavaStyle), testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object ProgramTransformerUnitTest {
  private val loopIterationMultiplier = 1
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
      s"""class Test {
         |  int execute(int n, int INDEX_VARIABLE)
         |  {
         |    int USE_COUNT = 0;
         |    int USE = 0;
         |    if (n < 5)
         |    {
         |      return USE;
         |    }
         |    else
         |    {
         |      ;
         |    }
         |    // int R = 0;
         |    int i = 0;
         |    while (i < n)
         |    {
         |      USE = 5;
         |      USE_COUNT = USE_COUNT + 1;
         |      if (INDEX_VARIABLE == USE_COUNT)
         |      {
         |        return USE;
         |      }
         |      else
         |      {
         |        ;
         |      }
         |      i = i + 1;
         |    }
         |    return USE;
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
         |    while (i < (n * 1))
         |    {
         |      i = i + 1;
         |    }
         |  }
         |}""".stripMargin),
    TestCase("Test 02", test02,
      s"""class Test {
         |  int execute(int[] array, int INDEX_VARIABLE)
         |  {
         |    int USE_COUNT = 0;
         |    int USE = 0;
         |    int x = arrayLength(array);
         |    x = arraySum(array);
         |    x = arrayRead(array, 3);
         |    return USE;
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
         |    while (i < (n * ${loopIterationMultiplier}))
         |    {
         |      i = i + 1;
         |    }
         |  }
         |}""".stripMargin),
  )

  private val naiveTransformationTests = List(
    TestCase("test 01", test01,
      """class Test {
        |  int execute(int n, int INDEX_VARIABLE)
        |  {
        |    int USE_COUNT = 0;
        |    int USE = 0;
        |    if (n < 5)
        |    {
        |      return USE;
        |    }
        |    else
        |    {
        |      ;
        |    }
        |    // int R = 0;
        |    int i = 0;
        |    while (i < n)
        |    {
        |      // use R 5
        |      i = i + 1;
        |    }
        |    return USE;
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
        |    while (i < (n * 1))
        |    {
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin),
  )
}