package brbo.backend.refiner

import brbo.common.cfg.CFGNode
import brbo.common.string.StringFormatUtils

object PrintPath {
  def pathToString(path: List[CFGNode]): String = {
    val nodes = path.zipWithIndex.map({ case (node, index: Int) => s"  [${StringFormatUtils.integer(index)}] ${node.toString}" }).mkString("\n")
    s"Path:\n$nodes"
  }
}
