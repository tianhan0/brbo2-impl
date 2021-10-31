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
  def getSplitUseInstances(use: Use, condition: BrboExpr): Map[Int, Set[Int]] = {
    groupIDs.get(use.groupId.get) match {
      case Some(newGroupIds) => // This use splits
        var map: Map[Int, Set[Int]] = Map()
        path.indices.foreach({
          i =>
            path(i).value match {
              case Left(command) =>
                if (command == use) {
                  splitUses(i) match {
                    case UseNode(_, groupId) =>
                      assert(newGroupIds.contains(groupId))
                      map.get(groupId) match {
                        case Some(set) => map = map + (groupId -> (set + i))
                        case None => map = map + (groupId -> Set(i))
                      }
                    case _ =>
                  }
                } // Otherwise, this is a use instance that belongs to the same group but does not correspond to the given use command
              case Right(_) => throw new Exception("We should never split a conditional")
            }
        })
        assert(map.keySet == newGroupIds)
        map
      case None => Map[Int, Set[Int]]() // This use does not split
    }
  }

  def getResetInstances(reset: Reset, newGroupId: Option[Int], condition: BrboExpr): (Set[Int], Set[Int]) = {
    var keepSet: Set[Int] = Set()
    var removeSet: Set[Int] = Set()
    newGroupId match {
      case Some(newGroupId2) => // Look for reset instances for new groups
        assert(groupIDs.getOrElse(reset.groupId, throw new Exception).contains(newGroupId2))
        path.indices.foreach({
          i =>
            path(i).value match {
              case Left(command) =>
                if (command == reset) {
                  splitUses(i) match {
                    case ResetNode(_, groupId) =>
                      if (newGroupId2 == groupId) {
                        if (removeSet.contains(i)) removeSet = removeSet + i
                        else keepSet = keepSet + i
                      }
                    case _ =>
                  }
                }
              case Right(_) =>
            }
        })
      case None => // Look for reset instances for original groups
        path.indices.foreach({
          i =>
            path(i).value match {
              case Left(command) =>
                if (command == reset) {
                  if (removeResets.contains(i)) removeSet = removeSet + i
                  else keepSet = keepSet + i
                }
              case Right(_) =>
            }
        })
    }
    (keepSet, removeSet)
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

abstract class Replace(val node: CFGNode, val groupId: Int)

case class UseNode(override val node: CFGNode, override val groupId: Int) extends Replace(node, groupId) {
  val use: Use = node.value.left.get.asInstanceOf[Use]

  override def toString: String = use.prettyPrintToCFG
}

case class ResetNode(override val node: CFGNode, override val groupId: Int) extends Replace(node, groupId) {
  val reset: Reset = node.value.left.get.asInstanceOf[Reset]

  override def toString: String = reset.prettyPrintToCFG
}