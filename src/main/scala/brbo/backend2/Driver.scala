package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.learning.TraceClustering
import brbo.common.ast.{BoundAssertion, BrboProgram}
import brbo.common.{CommandLineArguments, MyLogger}

class Driver(arguments: CommandLineArguments, program: BrboProgram) {
  private val logger = MyLogger.createLogger(classOf[Driver], arguments.getDebugMode)

  def verify(boundAssertion: BoundAssertion): Unit = {
    val traces = generateTraces()
    val clusters = clusterTraces(traces)
    clusters.foreach(cluster => logger.info(s"$cluster"))
  }

  def generateTraces(): List[Interpreter.Trace] = {
    logger.info(s"Step 1: Generate traces")
    Fuzzer.fuzz(program, arguments.getDebugMode)
  }

  def clusterTraces(traces: List[Interpreter.Trace]): Iterable[List[Interpreter.Trace]] = {
    logger.info("Step 2: Cluster similar traces")
    val costTraces = traces.map(t => t.costTrace)
    val groupedCostTraces = TraceClustering.groupSameTraces(costTraces)
    val representativeTraces = TraceClustering.selectRepresentativeTraces(groupedCostTraces)
    val distanceMatrix = TraceClustering.distanceMatrix(representativeTraces.keys.toList, substitutionPenalty = 100)
    val clusterLabels = TraceClustering.cluster(distanceMatrix, arguments.getDebugMode)
    costTraces.zip(traces).zip(clusterLabels).groupBy({
      case (_, label) => label
    }).values.map(list => list.map({case((_, trace), _) => trace}))
  }
}
