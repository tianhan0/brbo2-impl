package brbo.backend.learning

import brbo.backend.interpreter.Interpreter

object TraceClustering {
  def distanceMatrix(traces: List[Interpreter.CostTrace], substitutionPenalty: Int): List[List[Int]] = {
    traces.map({
      left =>
        traces.map({
          right => distance(left, right, substitutionPenalty)
        })
    })
  }

  private def distance(left: Interpreter.CostTrace, right: Interpreter.CostTrace, substitutionPenalty: Int): Int = {
    // Since permuting a trace has no cost, we eliminate the orders from traces
    val leftCostTrace: Set[String] = costTraceToSet(left)
    val rightCostTrace: Set[String] = costTraceToSet(right)
    // Common elements between traces have no cost
    val diff1 = leftCostTrace.diff(rightCostTrace)
    val diff2 = rightCostTrace.diff(leftCostTrace)
    // For uncommon elements (e.g., {x, y} and {a, b, c}), we first substitute (e.g., x becomes a and y becomes c)
    // and then insert (e.g., insert c)
    Math.min(diff1.size, diff2.size) * substitutionPenalty
  }

  private def costTraceToSet(trace: Interpreter.CostTrace): Set[String] = {
    trace.nodes.foldLeft(Set(): Set[String])({
      case (soFar, node) => node match {
        case Interpreter.UseNode(use, _) => soFar + s"${use.printToIR()}${use.uuid}"
        case _ => soFar
      }
    })
  }
}
