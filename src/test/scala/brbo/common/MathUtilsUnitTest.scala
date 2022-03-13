package brbo.common

import brbo.TestCase
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class MathUtilsUnitTest extends AnyFlatSpec {
  MathUtilsUnitTest.kGroupsFromNUnitTest.foreach({
    testCase =>
      val input = testCase.input.asInstanceOf[(Int, Int)]
      val result = MathUtils.kGroupsFromN(input._1, 0, input._2 - 1)
      assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, s"`${testCase.name}` failed!"))
  })

  MathUtilsUnitTest.generateUniqueSequencesUnitTest.foreach({
    testCase =>
      val input = testCase.input.asInstanceOf[(Int, Int)]
      val result = MathUtils.generateUniqueSequences(input._1, 0, input._2 - 1)
      assert(StringCompare.ignoreWhitespaces(result, testCase.expectedOutput, s"`${testCase.name}` failed!"))
  })
}

object MathUtilsUnitTest {
  val kGroupsFromNUnitTest: List[TestCase] = {
    val test01 = (2, 2)
    val test02 = (2, 3)
    val test03 = (3, 4)
    List(
      TestCase("Test 01", test01, """List(0, 1)
                                    |List(1, 0)""".stripMargin),
      TestCase("Test 02", test02, """List(0, 1)
                                    |List(0, 2)
                                    |List(1, 0)
                                    |List(1, 2)
                                    |List(2, 0)
                                    |List(2, 1)""".stripMargin),
      TestCase("Test 03", test03, """List(0, 1, 2)
                                    |List(0, 1, 3)
                                    |List(0, 2, 1)
                                    |List(0, 2, 3)
                                    |List(0, 3, 1)
                                    |List(0, 3, 2)
                                    |List(1, 0, 2)
                                    |List(1, 0, 3)
                                    |List(1, 2, 0)
                                    |List(1, 2, 3)
                                    |List(1, 3, 0)
                                    |List(1, 3, 2)
                                    |List(2, 0, 1)
                                    |List(2, 0, 3)
                                    |List(2, 1, 0)
                                    |List(2, 1, 3)
                                    |List(2, 3, 0)
                                    |List(2, 3, 1)
                                    |List(3, 0, 1)
                                    |List(3, 0, 2)
                                    |List(3, 1, 0)
                                    |List(3, 1, 2)
                                    |List(3, 2, 0)
                                    |List(3, 2, 1)""".stripMargin),
    )
  }

  val generateUniqueSequencesUnitTest: List[TestCase] = {
    val test01 = (2, 2)
    val test02 = (3, 2)
    val test03 = (4, 2)
    val test04 = (2, 3)
    val test05 = (3, 3)

    List(
      TestCase("Test 01", test01, """List(0, 0)
                                    |List(0, 1)""".stripMargin),
      TestCase("Test 02", test02, """List(0, 0, 0)
                                    |List(0, 0, 1)
                                    |List(0, 1, 0)
                                    |List(0, 1, 1)""".stripMargin),
      TestCase("Test 03", test03, """List(0, 0, 0, 0)
                                    |List(0, 0, 0, 1)
                                    |List(0, 0, 1, 0)
                                    |List(0, 0, 1, 1)
                                    |List(0, 1, 0, 0)
                                    |List(0, 1, 0, 1)
                                    |List(0, 1, 1, 0)
                                    |List(0, 1, 1, 1)""".stripMargin),
      TestCase("Test 04", test04, """List(0, 0)
                                    |List(0, 1)""".stripMargin),
      TestCase("Test 05", test05, """List(0, 0, 0)
                                    |List(0, 0, 1)
                                    |List(0, 1, 0)
                                    |List(0, 1, 1)
                                    |List(0, 1, 2)""".stripMargin),
    )
  }
}