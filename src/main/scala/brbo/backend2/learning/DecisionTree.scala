package brbo.backend2.learning

import brbo.common.Print
import brbo.common.ast.{BrboExpr, BrboExprUtils, Number}
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads.{min, _}
import play.api.libs.json.{JsPath, Json, Reads}

import scala.collection.immutable.Queue

object DecisionTree {
  private abstract class TreeRawNode

  private case class LeafRawNode(nodeID: Int, classID: Int) extends TreeRawNode

  private case class InternalRawNode(nodeID: Int, leftNodeID: Int, rightNodeID: Int, threshold: Float, featureID: Int) extends TreeRawNode

  private implicit val leafRawNodeReads: Reads[LeafRawNode] = (
    (JsPath \ "nodeID").read[Int](min(0)) and
      (JsPath \ "classID").read[Int](min(0))
    ) (LeafRawNode.apply _)

  private implicit val internalRawNodeReads: Reads[InternalRawNode] = (
    (JsPath \ "nodeID").read[Int](min(0)) and
      (JsPath \ "leftNodeID").read[Int](min(0)) and
      (JsPath \ "rightNodeID").read[Int](min(0)) and
      (JsPath \ "threshold").read[Float] and
      (JsPath \ "featureID").read[Int](min(0))
    ) (InternalRawNode.apply _)

  abstract class TreeNode(val id: Int) {
    def print(indent: Int): String
  }

  case class LeafNode(override val id: Int, classID: Int) extends TreeNode(id) {
    def print(indent: Int): String = {
      val indentString = " " * indent
      s"${indentString}LeafNode(id=$id, classID=$classID)"
    }
  }

  case class InternalNode(override val id: Int, left: TreeNode, right: TreeNode, threshold: Float, featureID: Int) extends TreeNode(id) {
    override def toString: String = s"InterNode(id=$id, left=$left, right=$right, threshold=$threshold, featureID=$featureID)"

    def print(indent: Int): String = {
      val indentString = " " * indent
      val leftString = s"${left.print(indent + 2)} (if feature[$featureID] <= $threshold)"
      val rightString = s"${right.print(indent + 2)} (if feature[$featureID] > $threshold)"
      s"${indentString}InterNode(id=$id, threshold=$threshold, featureID=$featureID)\n$leftString\n$rightString"
    }

    private val isInteger = (threshold % 1) == 0

    def getLeftPredicate(features: List[BrboExpr]): BrboExpr = {
      BrboExprUtils.lessThanOrEqualTo(features(featureID), Number(threshold.floor.toInt))
    }

    def getRightPredicate(features: List[BrboExpr]): BrboExpr = {
      if (isInteger) {
        BrboExprUtils.greaterThan(features(featureID), Number(threshold.toInt))
      } else {
        BrboExprUtils.greaterThanOrEqualTo(features(featureID), Number(threshold.ceil.toInt))
      }
    }
  }

  def parse(string: String): TreeClassifier = {
    val parsed = Json.parse(string)
    val leaves = parsed("leaves").as[List[LeafRawNode]]
    val nonLeaves = parsed("non_leaves").as[List[InternalRawNode]]
    val classes = parsed("classes").as[List[String]]
    TreeClassifier(root = buildTree(leaves, nonLeaves), labels = classes)
  }

  case class TreeClassifier(root: TreeNode, labels: List[String]) extends Print {
    def print(): String = {
      s"Labels: $labels\nTree:\n${root.print(indent = 0)}"
    }
  }

  private def buildTree(leaves: List[LeafRawNode], nonLeaves: List[InternalRawNode]): TreeNode = {
    var map: Map[Int, TreeNode] = Map()
    var potentialRoots: Set[Int] = Set()
    leaves.foreach({
      leaf =>
        map = map + (leaf.nodeID -> LeafNode(leaf.nodeID, leaf.classID))
        potentialRoots = potentialRoots + leaf.nodeID
    })
    var queue: Queue[InternalRawNode] = Queue(nonLeaves: _*)
    while (queue.nonEmpty) {
      val (head, newQueue) = queue.dequeue
      queue = newQueue

      (map.get(head.leftNodeID), map.get(head.rightNodeID)) match {
        case (Some(left), Some(right)) =>
          val node = InternalNode(head.nodeID, left, right, head.threshold, head.featureID)
          map = map + (head.nodeID -> node)
          potentialRoots = potentialRoots + head.nodeID - left.id - right.id
        case _ =>
          queue = queue.enqueue(head)
      }
    }
    assert(potentialRoots.size == 1)
    map(potentialRoots.head)
  }
}
