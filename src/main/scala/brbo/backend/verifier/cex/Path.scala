package brbo.backend.verifier.cex

import brbo.common.TypeUtils.BrboType.BOOL
import brbo.common.ast.PrettyPrintToC
import brbo.common.cfg.CFGNode

case class Path(pathNodes: List[CFGNode]) extends PrettyPrintToC {
  pathNodes.map(pathNode => pathNode.value).foreach({
    case Left(_) =>
    case Right(expr) => assert(expr.typ == BOOL)
  })

  override def prettyPrintToC(indent: Int): String = {
    pathNodes.map({ node => node.toString }).mkString("\n")
  }
}
