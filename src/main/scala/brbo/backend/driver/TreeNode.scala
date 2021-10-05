package brbo.backend.driver

import brbo.backend.driver.NodeStatus.NodeStatus
import brbo.backend.refiner.Refinement
import brbo.backend.verifier.cex.Path
import brbo.common.ast.BrboProgram

import scala.collection.mutable

/**
 *
 * @param program        A (refined) program
 * @param counterexample The counterexample path generated from this program, if exists
 * @param refinement     The refinement that leads to this program (when synthesized with its parent program)
 * @param parent         The parent node
 * @param knownChildren  The children nodes that we already know
 */
case class TreeNode(program: BrboProgram, counterexample: Option[Path], refinement: Option[Refinement],
                    parent: Option[TreeNode], knownChildren: mutable.Map[TreeNode, NodeStatus], id: Int) {
  assert(refinement.isEmpty == parent.isEmpty)
}
