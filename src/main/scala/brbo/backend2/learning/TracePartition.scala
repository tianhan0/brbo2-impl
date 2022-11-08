package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter.Trace

object TracePartition {
  def selectRepresentatives(traces: Iterable[Trace]): List[Trace] = {
    val sorted = traces.toList.sortWith({
      case (trace1, trace2) => trace1.nodes.length <= trace2.nodes.length
    })
    val chosenIndex = (sorted.length * 0.8).toInt
    val indexRange = Range.inclusive(chosenIndex - 3, chosenIndex + 3).intersect(Range(0, sorted.length))
    if (indexRange.isEmpty) Nil
    else sorted.slice(indexRange.head, indexRange.last)
  }
}
