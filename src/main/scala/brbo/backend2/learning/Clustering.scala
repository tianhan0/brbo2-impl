package brbo.backend2.learning

import brbo.backend2.learning.ScriptRunner.Algorithm
import play.api.libs.json.Json

object Clustering {
  /**
   *
   * @param dataMatrix When the algorithm is Optics, the data is a (n x n) matrix
   *                   that represents the distances between any two data points
   *                   for n data points.
   *                   When the algorithm is KMeans, the data is a (n x m) matrix
   *                   for n data points to cluster. Each data point is represented
   *                   by a vector of length m.
   * @param algorithm  The algorithm to use.
   * @param debugMode  Whether to print extra logging information.
   * @return
   */
  def cluster(dataMatrix: List[List[Int]], algorithm: Algorithm, debugMode: Boolean): Option[List[Int]] = {
    val inputFileContent: String = Json.obj(("data", dataMatrix)).toString()
    ScriptRunner.run(inputFileContent, algorithm, debugMode) match {
      case Some(outputFileContents) =>
        val parsed = Json.parse(outputFileContents)
        Some(parsed("labels").as[List[Int]])
      case None => None
    }
  }
}
