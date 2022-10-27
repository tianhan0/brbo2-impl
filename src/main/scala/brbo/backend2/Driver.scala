package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.CostTrace
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

  def clusterTraces(traces: List[Interpreter.Trace]): Iterable[List[Interpreter.Trace]] = {
    logger.info("Step 2: Cluster similar traces")
    val costTraces: List[CostTrace] = traces.map(t => t.costTrace)
    val groupedCostTraces: Iterable[List[CostTrace]] = TraceClustering.groupZeroDistanceTraces(costTraces)
    val representativeTraces: Map[CostTrace, List[CostTrace]] =
      TraceClustering.selectRepresentativeTraces(groupedCostTraces)
    val distanceMatrix: List[List[Int]] =
      TraceClustering.distanceMatrix(representativeTraces.keys.toList, substitutionPenalty = 100)
    val clusterLabels: List[Int] = TraceClustering.cluster(distanceMatrix, debugMode)
    costTraces.zip(traces).zip(clusterLabels).groupBy({
      case (_, label) => label
    }).values.map(list => list.map({ case ((_, trace), _) => trace }))
  }
}
