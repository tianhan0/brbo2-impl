package brbo.backend.verifier.cex

import brbo.common.TypeUtils.BrboType.BOOL
import brbo.common.ast.PrettyPrintToC

case class Path(pathNodes: List[PathNode]) extends PrettyPrintToC {
  pathNodes.map(pathNode => pathNode.node.value).foreach({
    case Left(_) =>
    case Right(expr) => assert(expr.typ == BOOL)
  })

  override def prettyPrintToC(indent: Int): String = {
    pathNodes.map({ node => node.toString }).mkString("\n")
  }
}
