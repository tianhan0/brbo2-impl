package brbo.backend2.learning

import brbo.TestCase
import brbo.backend2.learning.Clustering.Optics
import brbo.common.string.StringCompare
import org.scalatest.flatspec.AnyFlatSpec

class ClusteringUnitTest extends AnyFlatSpec {
  "Clustering a distance matrix" should "work" in {
    ClusteringUnitTest.clusterTests.foreach({
      testCase =>
        val labels = Clustering.cluster(testCase.input.asInstanceOf[List[List[Int]]], Optics(maxEps = None), debugMode = false)
        StringCompare.ignoreWhitespaces(labels, testCase.expectedOutput, s"${testCase.name} failed")
    })
  }

  "Writing matrices into JSON" should "be correct" in {
    ClusteringUnitTest.jsonWriteTests.foreach({
      testCase =>
        val json = Clustering.matrixToJsonString(testCase.input.asInstanceOf[List[List[Int]]])
        StringCompare.ignoreWhitespaces(json, testCase.expectedOutput, s"${testCase.name} failed")
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

  val jsonWriteTests: List[TestCase] = {
    val matrix1 = List(List(1, 2), List(3, 4, 5, 6))
    List(TestCase("matrix1", matrix1, """{"data":[[1,2],[3,4,5,6]]}"""))
  }
}