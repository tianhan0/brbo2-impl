package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, Trace}
import brbo.backend2.learning.ScriptRunner.Optics
import brbo.backend2.learning.{Clustering, TraceClustering}
import brbo.common.ast.{BoundAssertion, BrboProgram}
import brbo.common.{CommandLineArguments, MyLogger}

class Driver(arguments: CommandLineArguments, program: BrboProgram) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[Driver], debugMode)
  private val SUBSTITUTION_PENALTY = 100

  def verify(boundAssertion: BoundAssertion): Unit = {
    val rawTraces = generateTraces()
    val traces = clusterTraces(rawTraces)
    traces.foreach(trace => logger.info(s"$trace"))
  }

  def generateTraces(): List[Interpreter.Trace] = {
    logger.info(s"Step 1: Generate traces")
    Fuzzer.fuzz(program, debugMode)
  }

  def clusterTraces(traces: List[Interpreter.Trace]): Iterable[Trace] = {
    val STEP_2 = "Step 2: "
    logger.info(s"${STEP_2}Cluster similar traces: ${traces.length} traces")
    val costTraces: List[CostTrace] = traces.map(t => t.costTrace)

    logger.info(s"${STEP_2}Group traces with zero distance")
    val groupedCostTraces: Map[CostTrace, Set[CostTrace]] = TraceClustering.groupZeroDistanceTraces(costTraces)
    logger.info(s"${STEP_2}Found ${groupedCostTraces.size} groups of traces")
    val representativeCostTraces = groupedCostTraces.keys
    representativeCostTraces.foreach(t => logger.traceOrError(s"\n${t.print()}"))

    logger.info(s"${STEP_2}Compute a distance matrix")
    val distanceMatrix: List[List[Int]] =
      TraceClustering.distanceMatrix(representativeCostTraces.toList, SUBSTITUTION_PENALTY)

    logger.info(s"${STEP_2}Cluster traces")
    val clusterLabels: List[Int] = Clustering.cluster(distanceMatrix, Optics(Some(10)), debugMode) match {
      case Some(labels) => labels
      case None =>
        logger.info(s"Put all traces into the same cluster")
        List.fill(representativeCostTraces.size)(0)
    }

    val labelMap: Map[Int, List[((CostTrace, Trace), Int)]] =
      costTraces.zip(traces).zip(clusterLabels).groupBy({ case (_, label) => label })
    val outlierTraces = labelMap.get(-1) match {
      case Some(outliers) =>
        logger.info(s"${STEP_2}Found ${outliers.size} outlier traces")
        outliers.map({ case ((_, trace), _) => trace })
      case None => Nil
    }
    val clusters: Iterable[List[((CostTrace, Trace), Int)]] = (labelMap - (-1)).values
    logger.info(s"${STEP_2}Found ${clusters.size} trace clusters")

    logger.info(s"${STEP_2}Select representative traces from every cluster")
    clusters.map({
      list =>
        val traces = list.map({ case ((_, trace), _) => trace })
        TraceClustering.selectRepresentativeTrace(traces)
    }).toList ::: outlierTraces
  }
}
