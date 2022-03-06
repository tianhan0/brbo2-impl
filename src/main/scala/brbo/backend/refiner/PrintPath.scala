package brbo.backend.refiner

import brbo.common.StringFormatUtils
import brbo.common.cfg.CFGNode

object PrintPath {
  def pathToString(path: List[CFGNode]): String = {
    val nodes = path.zipWithIndex.map({ case (node, index: Int) => s"  [${StringFormatUtils.integer(index)}] ${node.toString}" }).mkString("\n")
    s"Path:\n$nodes"
  }
}
