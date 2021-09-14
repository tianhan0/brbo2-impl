package brbo.backend.verifier.cex

import brbo.common.ast.BrboFunction
import brbo.common.cfg.CFGNode

case class PathNode(node: CFGNode, brboFunction: BrboFunction) {
  override def toString: String = s"${node.prettyPrintToC()} [Function `${brboFunction.identifier}`]"
}
