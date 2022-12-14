package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter.Trace
import brbo.common.MyLogger

object TracePartition {
  private val logger = MyLogger.createLogger(TracePartition.getClass, debugMode = false)
  private val numberOfTraces = 1

  def selectRepresentatives(userProvidedTraces: List[Trace], fuzzerGeneratedTraces: List[Trace]): Map[Trace, List[Trace]] = {
    val prefix = "Step 2:"
    logger.info(s"$prefix Assume all traces are similar")
    if (userProvidedTraces.isEmpty) {
      logger.info(s"$prefix Select representatives from fuzzer generated traces")
      return select(fuzzerGeneratedTraces)
    }
    logger.info(s"$prefix Select representatives from user-provided traces")
    select(userProvidedTraces).map({
      case (trace, similarTraces) =>
        // Ensure the user-provided traces will be used for testing generality
        (trace, similarTraces ::: fuzzerGeneratedTraces)
    })
  }

  private def select(traces: List[Trace]): Map[Trace, List[Trace]] = {
    val sorted = traces.sortWith({
      case (trace1, trace2) => trace1.nodes.length < trace2.nodes.length
    })
    // It is best to choose a trace that starts to show patterns (compared with shorter traces, such that
    // the pattern is generalizable), but is not too long (to avoid over-fitting)
    val chosenIndex = (sorted.length * 0.0).toInt
    val indexRange = Range.inclusive(chosenIndex, chosenIndex + numberOfTraces - 1).intersect(Range(0, sorted.length))
    logger.info(s"Choose traces within range $indexRange from ${sorted.length} traces")
    if (indexRange.isEmpty) Map()
    else {
      indexRange.map({
        index =>
          // Pairs of traces and similar traces (that will be used for testing generality)
          val similarTraces = sorted.slice(0, index) ::: sorted.slice(index + 1, sorted.length)
          (sorted(index), similarTraces)
      }).toMap
    }
  }
}
