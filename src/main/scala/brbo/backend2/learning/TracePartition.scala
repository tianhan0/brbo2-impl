package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter.Trace
import brbo.common.MyLogger

object TracePartition {
  private val logger = MyLogger.createLogger(TracePartition.getClass, debugMode = false)
  private val numberOfTraces = 1

  def selectRepresentatives(traces: Iterable[Trace]): Map[Trace, Iterable[Trace]] = {
    val sorted = traces.toList.sortWith({
      case (trace1, trace2) => trace1.nodes.length < trace2.nodes.length
    })
    // It is best to choose a trace that starts to show patterns (compared with shorter traces, such that
    // the pattern is generalizable), but is not too long (to avoid over-fitting)
    val chosenIndex = (sorted.length * 0.2).toInt
    val indexRange = Range.inclusive(chosenIndex, chosenIndex + numberOfTraces - 1).intersect(Range(0, sorted.length))
    logger.info(s"Assume all traces are similar")
    logger.info(s"Choose traces within range $indexRange from ${sorted.length} traces")
    if (indexRange.isEmpty) Map()
    else {
      indexRange.map({
        index =>
          // Pairs of traces and similar traces
          val similarTraces = if (index + 1 >= sorted.length) Nil else sorted.slice(index + 1, sorted.length)
          // val similarTraces = traces.toSet - sorted(index)
          (sorted(index), similarTraces)
      }).toMap
    }
  }
}
