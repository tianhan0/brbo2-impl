package brbo.backend2.qfuzz

import brbo.common.string.StringCompare
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.scalatest.flatspec.AnyFlatSpec

class ProgramTransformationUnitTest extends AnyFlatSpec {
  "Transforming a program for QFuzz" should "be correct" in {
    val targetProgram = BasicProcessor.getTargetProgram("Test", ProgramTransformationUnitTest.test01)
    val result = ProgramTransformation.transform(targetProgram.program)
    StringCompare.ignoreWhitespaces(
      result.mainFunction.printToC(indent = 0),
      """void execute(int n, int INDEX_VARIABLE)
        |{
        |  if (8 < n)
        |  {
        |    return;
        |  }
        |  else
        |  {
        |    ;
        |  }
        |  int R = 0;
        |  {
        |    int i = 0;
        |    while (i < n)
        |    {
        |      {
        |        {
        |          use(5);
        |          INDEX_VARIABLE = INDEX_VARIABLE - 1;
        |          if (INDEX_VARIABLE == 0)
        |          {
        |            return;
        |          }
        |          else
        |          {
        |            ;
        |          }
        |        }
        |      }
        |      i = i + 1;
        |    }
        |  }
        |}""".stripMargin
    )
    StringCompare.ignoreWhitespaces(
      result.allFunctions.find(function => function.identifier == ProgramTransformation.USE_FUNCTION_NAME).get.printToC(indent = 0),
      """void use(int n)
        |{
        |  int i = 0;
        |  while (i < n)
        |  {
        |    i = i + 1;
        |  }
        |}""".stripMargin
    )
  }
}

object ProgramTransformationUnitTest {
  private val test01 =
    s"""class Test {
       |  void ${TargetProgram.MAIN_FUNCTION}(int n) {
       |    int R = 0;
       |    for (int i = 0; i < n; i++) {
       |      R = R + 5;
       |    }
       |  }
       |}""".stripMargin
}