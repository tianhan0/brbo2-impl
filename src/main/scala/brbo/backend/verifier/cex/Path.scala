package brbo.backend.verifier.cex

import brbo.common.BrboType.BOOL
import brbo.common.ast.BrboFunction
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

  // Get segments of the given group in the given function
  def getSegments(groupId: Int, function: BrboFunction): List[Segment] = {
    val nodes = pathNodes.filter(node => node.function == function)
    if (nodes.isEmpty)
      return Nil
    assert(nodes.head.isReset(None, Some(function)), s"First segment of group `$groupId` does not begin with a reset in `$toString`")
    var results: List[Segment] = Nil
    var begin = 0
    var end = 0
    var i = 1
    while (i < nodes.size) {
      if (nodes(i).isReset(Some(groupId), Some(function))) {
        results = Segment(this, begin, end) :: results
        begin = i
        end = i
      }
      else {
        end = end + 1
      }
      i = i + 1
    }
    results = Segment(this, begin, end) :: results
    results.reverse
  }
}

case class Segment(path: Path, begin: Int, end: Int)