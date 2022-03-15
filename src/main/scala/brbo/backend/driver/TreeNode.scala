package brbo.backend.driver

import brbo.backend.driver.NodeStatus.NodeStatus
import brbo.backend.refiner.Refinement
import brbo.backend.verifier.cex.Path
import brbo.common.ast.BrboProgram

/**
 *
 * @param program         A (refined) program
 * @param counterexamples The counterexample paths generated from this program
 * @param refinement      The refinement that leads to this program (when synthesized with its parent program)
 */
case class TreeNode(program: BrboProgram, counterexamples: Set[Path], refinement: Option[Refinement], id: Int) {
  private var nodeStatus: Option[NodeStatus] = None

  def setNodeStatus(status: NodeStatus): Unit = {
    nodeStatus = Some(status)
  }

  def getNodeStatus: Option[NodeStatus] = nodeStatus
}
