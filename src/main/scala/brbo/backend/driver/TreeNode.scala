package brbo.backend.driver

import brbo.backend.driver.NodeStatus.NodeStatus
import brbo.backend.refiner.Refinement
import brbo.backend.verifier.cex.Path
import brbo.common.ast.BrboProgram

/**
 *
 * @param program        A (refined) program
 * @param counterexample The counterexample path generated from this program, if exists
 * @param refinement     The refinement that leads to this program (when synthesized with its parent program)
 */
case class TreeNode(program: BrboProgram, counterexample: Option[Path], refinement: Option[Refinement], id: Int) {
  private var nodeStatus: Option[NodeStatus] = None

  def setNodeStatus(status: NodeStatus): Unit = {
    nodeStatus = Some(status)
  }

  def getNodeStatus: Option[NodeStatus] = nodeStatus
}
