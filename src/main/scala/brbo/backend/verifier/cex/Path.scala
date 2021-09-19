package brbo.backend.verifier.cex

import brbo.common.TypeUtils.BrboType.BOOL
import brbo.common.cfg.CFGNode

case class Path(pathNodes: List[CFGNode]) {
  pathNodes.map(pathNode => pathNode.value).foreach({
    case Left(_) =>
    case Right(expr) => assert(expr.typ == BOOL)
  })

  override def toString: String = {
    val nodes = pathNodes.map({ node => node.toString }).mkString("\n  ")
    s"Path:\n  $nodes"
  }
}
