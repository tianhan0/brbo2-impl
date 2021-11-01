package brbo.backend.refiner

import brbo.common.ast.{Reset, Use}
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
  // A map from new group IDs to indices of use instances (of the given use command) in the path that
  // belong to the new group
  def getSplitUseInstances(useInOriginalPath: Use): Map[Int, Set[Int]] = {
    groupIDs.get(useInOriginalPath.groupId.get) match {
      case Some(newGroupIds) => // This use splits
        var map: Map[Int, Set[Int]] = Map()
        path.indices.foreach({
          i =>
            path(i).value match {
              case Left(command) =>
                if (command == useInOriginalPath) {
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

  // A map from group IDs to indices of reset instances (of the given reset command) in the path that
  // belong to the group and are kept and removed
  def getResetInstances2(resetInOriginalPath: Reset): Map[Int, (Set[Int], Set[Int])] = {
    val thisGroupId = resetInOriginalPath.groupId
    val result = path.indices.foldLeft(Map[Int, (Set[Int], Set[Int])]())({
      (acc, i) =>
        path(i).value match {
          case Left(command) =>
            if (command == resetInOriginalPath) {
              val newGroupId = splitUses(i) match {
                case ResetNode(_, newGroupId) => newGroupId // Replaced
                case _ => thisGroupId // Not replaced
              }
              val (keepSet: Set[Int], removeSet: Set[Int]) = acc.get(thisGroupId) match {
                case Some((keepSet, removeSet)) => (keepSet, removeSet)
                case None => (Set(), Set())
              }
              val (newKeepSet, newRemoveSet) = {
                if (removeResets.contains(i)) (keepSet, removeSet + i)
                else (keepSet + i, removeSet)
              }
              acc + (newGroupId -> (newKeepSet, newRemoveSet))
            }
            else acc
          case Right(_) => acc
        }
    })
    result.size match {
      case 0 =>
      case 1 => assert(result.head._1 == thisGroupId) // The given reset command is not split
      case _ => result.keys.toSet.subsetOf(groupIDs(thisGroupId)) // The given reset command is split
    }
    result
  }

  def getResetInstances(resetInOriginalPath: Reset, newGroupId: Option[Int]): (Set[Int], Set[Int]) = {
    var keepSet: Set[Int] = Set()
    var removeSet: Set[Int] = Set()
    newGroupId match {
      case Some(newGroupId2) => // Look for reset instances for new groups
        assert(groupIDs.getOrElse(resetInOriginalPath.groupId, throw new Exception).contains(newGroupId2))
        path.indices.foreach({
          i =>
            path(i).value match {
              case Left(command) =>
                if (command == resetInOriginalPath) {
                  splitUses(i) match {
                    case ResetNode(_, groupId) =>
                      if (newGroupId2 == groupId) {
                        if (removeResets.contains(i)) removeSet = removeSet + i
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
                if (command == resetInOriginalPath) {
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
      case (acc, (i, replacement)) => acc.updated(i, replacement.newNode)
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

abstract class Replace(val newNode: CFGNode, val newGroupId: Int)

case class UseNode(newUse: CFGNode, override val newGroupId: Int) extends Replace(newUse, newGroupId) {
  val use: Use = newUse.value.left.get.asInstanceOf[Use]

  override def toString: String = use.prettyPrintToCFG
}

case class ResetNode(newReset: CFGNode, override val newGroupId: Int) extends Replace(newReset, newGroupId) {
  val reset: Reset = newReset.value.left.get.asInstanceOf[Reset]

  override def toString: String = reset.prettyPrintToCFG
}