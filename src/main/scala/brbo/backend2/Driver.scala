package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, Trace}
import brbo.backend2.learning.TraceClustering
import brbo.common.ast.{BoundAssertion, BrboProgram}
import brbo.common.{CommandLineArguments, MyLogger}

class Driver(arguments: CommandLineArguments, program: BrboProgram) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[Driver], debugMode)

  def verify(boundAssertion: BoundAssertion): Unit = {
    val traces = generateTraces()
    val clusters = clusterTraces(traces)
    clusters.foreach(cluster => logger.info(s"$cluster"))
  }

  def generateTraces(): List[Interpreter.Trace] = {
    logger.info(s"Step 1: Generate traces")
    Fuzzer.fuzz(program, debugMode)
  }

  def clusterTraces(traces: List[Interpreter.Trace]): Iterable[Trace] = {
    val STEP_2 = "Step 2: "
    logger.info(s"${STEP_2}Cluster similar traces: ${traces.length} traces")
    val costTraces: List[CostTrace] = traces.map(t => t.costTrace)

    logger.info(s"${STEP_2}Group traces with 0 distance")
    val groupedCostTraces: Map[CostTrace, Set[CostTrace]] = TraceClustering.groupZeroDistanceTraces(costTraces)
    logger.info(s"${STEP_2}Group traces with 0 distance: ${groupedCostTraces.size} groups")

    logger.info(s"${STEP_2}Compute a distance matrix")
    val distanceMatrix: List[List[Int]] =
      TraceClustering.distanceMatrix(groupedCostTraces.keys.toList, substitutionPenalty = 100)

    logger.info(s"${STEP_2}Clustering traces")
    val clusterLabels: List[Int] = TraceClustering.cluster(distanceMatrix, debugMode)
    val clusters: Iterable[List[((CostTrace, Trace), Int)]] = costTraces.zip(traces).zip(clusterLabels)
      .groupBy({ case (_, label) => label }).values
    logger.info(s"${STEP_2}Clustering traces: ${clusters.size} clusters")

    logger.info(s"${STEP_2}Select representative traces from every cluster")
    clusters.map({
      list =>
        val traces = list.map({case ((_, trace), _) => trace})
        TraceClustering.selectRepresentativeTrace(traces)
    })
  }
}
