package brbo.backend2.learning

import brbo.backend2.DecompositionDriver
import brbo.backend2.DecompositionDriver.InputTrace
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.common.MyLogger

object TraceSelection {
  private val logger = MyLogger.createLogger(TraceSelection.getClass, debugMode = false)
  private val numberOfTraces = 1

  def selectRepresentatives(traces: List[Trace],
                            inputTrace: InputTrace): Map[Trace, List[Trace]] = {
    inputTrace match {
      case DecompositionDriver.FuzzerGeneratedTrace => select(traces, sortByLengths = true)
      case DecompositionDriver.QFuzzGeneratedTrace => select(traces, sortByLengths = false)
      case DecompositionDriver.UserProvidedTrace => select(traces, sortByLengths = true)
      case _ => throw new Exception
    }
  }

  private def select(traces: List[Trace], sortByLengths: Boolean): Map[Trace, List[Trace]] = {
    val sorted = {
      if (sortByLengths) {
        // It is best to choose a trace that starts to show patterns (compared with shorter traces, such that
        // the pattern is generalizable), but is not too long (to avoid over-fitting)
        traces.sortWith({
          case (trace1, trace2) => trace1.nodes.length < trace2.nodes.length
        })
      } else
        traces
    }
    val chosenIndex = (sorted.length * 0.0).toInt
    val indexRange = Range.inclusive(chosenIndex, chosenIndex + numberOfTraces - 1).intersect(sorted.indices)
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
