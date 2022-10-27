package brbo.common

import brbo.TestCase
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class DisjointSetUnitTest extends AnyFlatSpec {
  "Constructing disjoint sets" should "be correct" in {
    DisjointSetUnitTest.tests.foreach({
      testCase =>
        val disjointSet = new DisjointSet[Int](
          (left, right) => left % 5 == right % 5,
          (left, right) => left >= right
        )
        testCase.input.asInstanceOf[Set[Int]].foreach(i => disjointSet.add(i))
        StringCompare.ignoreWhitespaces(disjointSet.getMap, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object DisjointSetUnitTest {
  private val test1 = Set(1, 1, 2, 3, 5, 10)
  private val test2 = Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  val tests: List[TestCase] = List(
    TestCase("test1", test1,
      """(1,Set(1))
        |(10,Set(5, 10))
        |(2,Set(2))
        |(3,Set(3))""".stripMargin),
    TestCase("test1", test2,
      """(10,Set(5, 10))
        |(6,Set(1, 6))
        |(7,Set(2, 7))
        |(8,Set(3, 8))
        |(9,Set(9, 4))""".stripMargin),
  )
}