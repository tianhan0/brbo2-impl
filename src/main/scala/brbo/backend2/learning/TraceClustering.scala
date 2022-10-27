package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, Trace}
import brbo.common.DisjointSet
import play.api.libs.json.Json

object TraceClustering {
  def groupZeroDistanceTraces(traces: List[CostTrace]): Map[CostTrace, Set[CostTrace]] = {
    val disjointSet = new DisjointSet[CostTrace](
      (left, right) => distance(left, right, substitutionPenalty = 1) == 0,
      betterRepresentative
    )
    traces.foreach(t => disjointSet.add(t))

    disjointSet.getMap
  }

  def selectRepresentativeCostTrace(traces: List[CostTrace]): CostTrace = {
    traces.sortWith({
      case (left, right) => betterRepresentative(left, right)
    }).head
  }

  private def betterRepresentative(left: CostTrace, right: CostTrace): Boolean = {
    val (diff1, diff2) = distanceInternal(left, right)
    // If left is {x, y} and right is {y, z, w}, then right is a better representative because its decomposition
    // is more easily applicable to left, by treating x as z (or w)
    diff1.size >= diff2.size
  }

  def selectRepresentativeTrace(traces: List[Trace]): Trace = {
    val selectedCostTrace = selectRepresentativeCostTrace(traces.map(t => t.costTrace))
    traces.find(t => t.costTrace == selectedCostTrace).get
  }

  def distanceMatrix(traces: List[CostTrace], substitutionPenalty: Int): List[List[Int]] = {
    traces.map({
      left =>
        traces.map({
          right => distance(left, right, substitutionPenalty)
        })
    })
  }

  def matrixToJsonString(distanceMatrix: List[List[Int]]): String = {
    val jsonObject = Json.obj(("data", distanceMatrix))
    jsonObject.toString()
  }

  private def distance(left: CostTrace, right: CostTrace, substitutionPenalty: Int): Int = {
    val (diff1, diff2) = distanceInternal(left, right)
    Math.min(diff1.size, diff2.size) * substitutionPenalty
  }

  private def distanceInternal(left: CostTrace, right: CostTrace): (Set[String], Set[String]) = {
    // Since permuting a trace has no cost, we eliminate the orders from traces
    val leftCostTrace: Set[String] = costTraceToSet(left)
    val rightCostTrace: Set[String] = costTraceToSet(right)
    // Common elements between traces have no cost. For example, between {x} and {x, y}, x is the common element and
    // hence we can ignore it when computing the distance
    val diff1 = leftCostTrace.diff(rightCostTrace)
    val diff2 = rightCostTrace.diff(leftCostTrace)
    // For uncommon elements (e.g., {x, y} and {a, b, c}), we first substitute (e.g., x becomes a and y becomes c)
    // and then insert (e.g., insert c)
    (diff1, diff2)
  }

  private def costTraceToSet(trace: CostTrace): Set[String] = {
    trace.nodes.foldLeft(Set(): Set[String])({
      case (soFar, node) => node match {
        case Interpreter.UseNode(use, _) => soFar + s"${use.printToIR()}${use.uuid}"
        case _ => soFar
      }
    })
  }
}
