package brbo.backend.refiner

import brbo.common.ast.{BrboExpr, Reset, Use}
import brbo.common.cfg.CFGNode

/**
 *
 * @param path         A path that this refinement applies to.
 * @param splitUses    A map from indices of the path to the replacements for the nodes at the indices.
 *                     The replacement is either a use command or a reset command for a new group.
 * @param removeResets Remove the resets at these indices from the path after splitting uses.
 * @param groupIDs     A map from old group IDs to new group IDs in the refined path.
 */
case class Refinement(path: List[CFGNode], splitUses: Map[Int, Replace], removeResets: Set[Int], groupIDs: Map[Int, Set[Int]]) {
  def getUseInstances(use: Use, condition: BrboExpr): Set[Set[Int]] = {
    ???
  }

  def getResetInstances(reset: Reset, newGroupId: Int, condition: BrboExpr): (Set[Int], Set[Int]) = {
    ???
  }

  def removeReset(index: Int): Refinement = Refinement(path, splitUses, removeResets + index, groupIDs)

  def getRefinedPath: List[CFGNode] = {
    val afterSplit: List[CFGNode] = splitUses.foldLeft(path)({
      case (acc, (i, replacement)) => acc.updated(i, replacement.node)
    })
    var result: List[CFGNode] = Nil
    afterSplit.indices.foreach({
      i => if (!removeResets.contains(i)) result = afterSplit(i) :: result
    })
    result.reverse
  }

  private val separator = "\n  "
  private val removedString = {
    val s = removeResets.toList.sorted.mkString(separator)
    s"Removed resets:$separator$s"
  }
  private val splitsString = {
    val s = splitUses.toList.map({
      case (index, replace) =>
        s"$index: $replace"
    }).sorted.mkString(separator)
    s"Splits:$separator$s"
  }

  override def toString: String = {
    val pathString = {
      val s = path.map({ node => node.toString }).mkString(separator)
      s"Path:$separator$s"
    }
    s"$pathString\n$splitsString\n$removedString"
  }

  def toStringNoPath: String = s"$splitsString\n$removedString"
}

abstract class Replace(val node: CFGNode, val groupID: Int)

case class UseNode(override val node: CFGNode, override val groupID: Int) extends Replace(node, groupID) {
  val use: Use = node.value.left.get.asInstanceOf[Use]

  override def toString: String = use.prettyPrintToCFG
}

case class ResetNode(override val node: CFGNode, override val groupID: Int) extends Replace(node, groupID) {
  val reset: Reset = node.value.left.get.asInstanceOf[Reset]

  override def toString: String = reset.prettyPrintToCFG
}