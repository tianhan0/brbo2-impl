package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, Trace}
import brbo.backend2.learning.ScriptRunner.{Algorithm, Optics, Precomputed}
import brbo.common.{DisjointSet, MyLogger}

object TraceClustering {
  private val logger = MyLogger.createLogger(TraceClustering.getClass, debugMode = false)
  private val SUBSTITUTION_PENALTY = 100

  def run(traces: List[Interpreter.Trace], debugMode: Boolean): Iterable[Trace] = {
    val STEP_NAME = "Step 2: "
    logger.info(s"${STEP_NAME}Cluster similar traces: ${traces.length} traces")
    val costTraces: List[CostTrace] = traces.map(t => t.costTrace)

    logger.info(s"${STEP_NAME}Group traces with zero distance")
    val groupedCostTraces: Map[CostTrace, Set[CostTrace]] = TraceClustering.groupZeroDistanceTraces(costTraces)
    logger.info(s"${STEP_NAME}Found ${groupedCostTraces.size} groups of traces")
    val representativeCostTraces = groupedCostTraces.keys
    representativeCostTraces.foreach(t => logger.traceOrError(s"\n${t.print()}"))

    logger.info(s"${STEP_NAME}Compute a distance matrix")
    val distanceMatrix: List[List[Int]] =
      TraceClustering.distanceMatrix(representativeCostTraces.toList, EditDistance(SUBSTITUTION_PENALTY))

    logger.info(s"${STEP_NAME}Cluster traces")
    val traceClusteringAlgorithm: Algorithm = Optics(Some(10), metric = Precomputed)
    val clusterLabels: List[Int] = Clustering.cluster(distanceMatrix, traceClusteringAlgorithm, debugMode) match {
      case Some(labels) => labels
      case None =>
        logger.info(s"Put all traces into the same cluster")
        List.fill(representativeCostTraces.size)(0)
    }

    val labelMap: Map[Int, List[((CostTrace, Trace), Int)]] =
      costTraces.zip(traces).zip(clusterLabels).groupBy({ case (_, label) => label })
    val outlierTraces = {
      traceClusteringAlgorithm match {
        case _: Optics =>
          labelMap.get(-1) match {
            case Some(outliers) =>
              logger.info(s"${STEP_NAME}Found ${outliers.size} outlier traces")
              outliers.map({ case ((_, trace), _) => trace })
            case None => Nil
          }
        case _ => Nil
      }
    }
    val clusters: Iterable[List[((CostTrace, Trace), Int)]] = (labelMap - (-1)).values
    logger.info(s"${STEP_NAME}Found ${clusters.size} trace clusters")

    logger.info(s"${STEP_NAME}Select representative traces from every cluster")
    clusters.map({
      list =>
        val traces = list.map({ case ((_, trace), _) => trace })
        TraceClustering.selectRepresentativeTrace(traces)
    }).toList ::: outlierTraces
  }

  def groupZeroDistanceTraces(traces: List[CostTrace]): Map[CostTrace, Set[CostTrace]] = {
    val disjointSet = new DisjointSet[CostTrace](
      (left, right) => editDistance(left, right, substitutionPenalty = 1) == 0,
      betterRepresentative
    )
    traces.foreach(t => disjointSet.add(t))

    disjointSet.getMap
  }

  def selectRepresentativeCostTrace(traces: List[CostTrace]): CostTrace = {
    traces.sortWith({
      case (left, right) => betterRepresentative(left, right)
    }).head
  }

  private def betterRepresentative(left: CostTrace, right: CostTrace): Boolean = {
    val (diff1, diff2) = distanceInternal(left, right)
    // If left is {x, y} and right is {y, z, w}, then right is a better representative because its decomposition
    // is more easily applicable to left, by treating x as z (or w)
    diff1.size > diff2.size
  }

  def selectRepresentativeTrace(traces: List[Trace]): Trace = {
    val selectedCostTrace = selectRepresentativeCostTrace(traces.map(t => t.costTrace))
    traces.find(t => t.costTrace == selectedCostTrace).get
  }

  abstract class SimilarityMetric

  case class EditDistance(substitutionPenalty: Int) extends SimilarityMetric

  object BagOfWords extends SimilarityMetric

  def distanceMatrix(traces: List[CostTrace], similarityMetric: SimilarityMetric): List[List[Int]] = {
    traces.map({
      left =>
        traces.map({
          right =>
            similarityMetric match {
              case BagOfWords => bagOfWordsDistance(left, right)
              case EditDistance(substitutionPenalty) => editDistance(left, right, substitutionPenalty)
              case _ => throw new Exception
            }
        })
    })
  }

  private def bagOfWordsDistance(left: CostTrace, right: CostTrace): Int = {
    val leftCostTrace: Set[String] = costTraceToSet(left)
    val rightCostTrace: Set[String] = costTraceToSet(right)
    val intersection = leftCostTrace.intersect(rightCostTrace)
    val union = leftCostTrace.union(rightCostTrace)
    val scale = 100
    (intersection.size.toDouble / union.size.toDouble * scale).toInt
  }

  private def editDistance(left: CostTrace, right: CostTrace, substitutionPenalty: Int): Int = {
    val (diff1, diff2) = distanceInternal(left, right)
    Math.min(diff1.size, diff2.size) * substitutionPenalty
  }

  private def distanceInternal(left: CostTrace, right: CostTrace): (Set[String], Set[String]) = {
    // Since permuting a trace has no cost, we eliminate the orders from traces
    val leftCostTrace: Set[String] = costTraceToSet(left)
    val rightCostTrace: Set[String] = costTraceToSet(right)
    // Common elements between traces have no cost. For example, between {x} and {x, y}, x is the common element and
    // hence we can ignore it when computing the distance
    val diff1 = leftCostTrace.diff(rightCostTrace)
    val diff2 = rightCostTrace.diff(leftCostTrace)
    // For uncommon elements (e.g., {x, y} and {a, b, c}), we first substitute (e.g., x becomes a and y becomes c)
    // and then insert (e.g., insert c)
    (diff1, diff2)
  }

  private def costTraceToSet(trace: CostTrace): Set[String] = {
    trace.nodes.foldLeft(Set(): Set[String])({
      case (soFar, node) => node match {
        case Interpreter.UseNode(use, _) => soFar + s"${use.printToIR()}${use.uuid}"
        case _ => soFar
      }
    })
  }
}
