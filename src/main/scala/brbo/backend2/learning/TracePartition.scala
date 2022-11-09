package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter.Trace
import brbo.common.MyLogger

object TracePartition {
  private val logger = MyLogger.createLogger(TracePartition.getClass, debugMode = false)
  private val beforeAndAfter = 1
  def selectRepresentatives(traces: Iterable[Trace]): List[Trace] = {
    val sorted = traces.toList.sortWith({
      case (trace1, trace2) => trace1.nodes.length <= trace2.nodes.length
    })
    val chosenIndex = (sorted.length * 0.8).toInt
    val indexRange = Range.inclusive(chosenIndex - beforeAndAfter, chosenIndex + beforeAndAfter).intersect(Range(0, sorted.length))
    logger.info(s"Assume all traces are similar")
    logger.info(s"Choose traces within range $indexRange")
    if (indexRange.isEmpty) Nil
    else sorted.slice(indexRange.head, indexRange.last)
  }
}
