package brbo.backend2.learning

import brbo.TestCase
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class ClusteringUnitTest extends AnyFlatSpec {
  "Clustering a distance matrix" should "work" in {
    ClusteringUnitTest.clusterTests.foreach({
      testCase =>
        val labels = Clustering.cluster(testCase.input.asInstanceOf[List[List[Int]]], debugMode = false)
        StringCompare.ignoreWhitespaces(labels, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }
}

object ClusteringUnitTest {
  val clusterTests: List[TestCase] = {
    val matrix1 = List(List(1, 2), List(3, 4))
    List(TestCase("matrix1", matrix1,
      """0
        |0""".stripMargin))
  }
}