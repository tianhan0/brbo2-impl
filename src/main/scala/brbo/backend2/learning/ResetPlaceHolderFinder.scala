package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter.{Trace, Transition}
import brbo.backend2.learning.Classifier.{GroupID, TableGenerationError}
import brbo.backend2.learning.SegmentClustering.Group
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
import com.ibm.wala.util.graph.NumberedGraph

object ResetPlaceHolderFinder {
  // From group IDs to the trace node indices into which resets need to be placed
  def indices(trace: Trace,
              groups: Map[GroupID, Group],
              throwIfNoResetPlaceHolder: Boolean,
              controlFlowGraph: ControlFlowGraph): Map[GroupID, Set[Int]] = {
    val resetPlaceHolderCandidates: Map[GroupID, Option[Command]] =
      asPostDominators(trace = trace, groups = groups, controlFlowGraph = controlFlowGraph)

    locations(trace, groups, resetPlaceHolderCandidates, throwIfNoResetPlaceHolder)
  }

  def asDominators(trace: Trace,
                   groups: Map[GroupID, Group],
                   controlFlowGraph: ControlFlowGraph): Map[GroupID, Option[Command]] = {
    asDominatorsHelper(
      trace = trace,
      groups = groups,
      graph = controlFlowGraph.walaGraph,
      entryNode = controlFlowGraph.entryNode,
      controlFlowGraph.nodesFromCommands
    )
  }

  // We often need resets to split costs from different loop iterations, for which
  // post-dominators are good candidates
  def asPostDominators(trace: Trace,
                       groups: Map[GroupID, Group],
                       controlFlowGraph: ControlFlowGraph): Map[GroupID, Option[Command]] = {
    val (reversedCopiedGraph, reversedRoot) = ControlFlowGraph.reverseGraph(controlFlowGraph)
    // ControlFlowGraph.printDotToPDF("abc", ControlFlowGraph.exportToDOT())
    asDominatorsHelper(
      trace = trace,
      groups = groups,
      graph = reversedCopiedGraph,
      entryNode = reversedRoot,
      controlFlowGraph.nodesFromCommands
    )
  }

  private def asDominatorsHelper(trace: Trace,
                                 groups: Map[GroupID, Group],
                                 graph: NumberedGraph[CFGNode],
                                 entryNode: CFGNode,
                                 nodesFromCommands: Set[Command] => Set[CFGNode]): Map[GroupID, Option[Command]] = {
    groups.map({
      case (groupID, group) =>
        val commands = group.getCommands(trace)
        val nodes = nodesFromCommands(commands.toSet)
        // Find a node that dominates all commands from the group
        val dominator = ControlFlowGraph.closestDominator(
          graph = graph,
          entryNode = entryNode,
          nodes = nodes,
          predicate = { node: CFGNode => node.command.isInstanceOf[ResetPlaceHolder] }
        )
        dominator match {
          case Some(dominator) => (groupID, Some(dominator.command))
          case None => (groupID, None)
        }
    })
  }

  private def locations(trace: Trace,
                        groups: Map[GroupID, Group],
                        resetPlaceHolderCandidates: Map[GroupID, Option[Command]],
                        throwIfNoResetPlaceHolder: Boolean): Map[GroupID, Set[Int]] = {
    val nodesWithIndex = trace.nodes.zipWithIndex
    groups.map({
      case (groupID, group) =>
        var resetPlaceHolderIndices: Set[Int] = Set()
        var i = 0
        while (i + 1 < group.segments.size) {
          val begin = {
            val current = group.segments(i)
            current.indices.last + 1
          }
          val end = {
            val next = group.segments(i + 1)
            next.indices.head - 1
          }
          val allPossibleResetPlaceHolders: Iterable[(ResetPlaceHolder, Int)] = nodesWithIndex.flatMap({
            case (node, index) =>
              val isResetPlaceHolder = node.lastTransition match {
                case Some(Transition(command, _)) => command.isInstanceOf[ResetPlaceHolder]
                case _ => false
              }
              if (isResetPlaceHolder && begin <= index && index <= end)
                Some((node.lastTransition.get.command.asInstanceOf[ResetPlaceHolder], index))
              else
                None
          })
          fromCandidates(allPossibleResetPlaceHolders, resetPlaceHolderCandidates(groupID)) match {
            case Some(index) => resetPlaceHolderIndices = resetPlaceHolderIndices + index
            case None =>
              val errorMessage = s"Failed to find a reset place holder between " +
                s"${groupID.print()}'s $i and ${i + 1} segment (index range: [$begin, $end])\n" +
                s"${SegmentClustering.printDecomposition(trace, groups)}"
              if (throwIfNoResetPlaceHolder) throw TableGenerationError(errorMessage)
          }
          i = i + 1
        }
        (groupID, resetPlaceHolderIndices)
    })
  }

  private def fromCandidates(allPossibleResetPlaceHolders: Iterable[(ResetPlaceHolder, Int)],
                             candidates: Option[Command]): Option[Int] = {
    if (allPossibleResetPlaceHolders.isEmpty || candidates.isEmpty)
      None
    else {
      // logger.trace(s"dominator: $dominator")
      // holders.foreach({ case (holder, i) => logger.trace(s"index $i: ${holder}")})
      // Let the reset place holder be the dominator
      allPossibleResetPlaceHolders.find({ case (resetPlaceHolder, _) => candidates.contains(resetPlaceHolder) }) match {
        case Some((_, index)) => Some(index)
        case None => None
      }
    }
  }
}
