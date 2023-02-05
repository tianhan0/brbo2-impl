package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter.Store
import brbo.backend2.learning.Classifier.{GeneralityTestGroup, GroupID}
import brbo.common.ast._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.Reads.{min, _}
import play.api.libs.json.{JsPath, Json, Reads}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object DecisionTree {
  private abstract class TreeRawNode

  private case class LeafRawNode(nodeID: Int, classID: Int) extends TreeRawNode

  private case class InternalRawNode(nodeID: Int, leftNodeID: Int, rightNodeID: Int, threshold: Float, featureID: Int) extends TreeRawNode

  private implicit val leafRawNodeReads: Reads[LeafRawNode] = (
    (JsPath \ "nodeID").read[Int](min(0)) and
      (JsPath \ "classID").read[Int](min(0))
    )(LeafRawNode.apply _)

  private implicit val internalRawNodeReads: Reads[InternalRawNode] = (
    (JsPath \ "nodeID").read[Int](min(0)) and
      (JsPath \ "leftNodeID").read[Int](min(0)) and
      (JsPath \ "rightNodeID").read[Int](min(0)) and
      (JsPath \ "threshold").read[Float] and
      (JsPath \ "featureID").read[Int](min(0))
    )(InternalRawNode.apply _)

  abstract class TreeNode(val id: Int) {
    def print(indent: Int, featureNames: List[String], classNames: List[String]): String

    def toAst(features: List[BrboExpr], classes: List[Label], leafType: LeafType): BrboAst

    def predicateCount(): Int
  }

  case class LeafNode(override val id: Int, classID: Int) extends TreeNode(id) {
    def print(indent: Int, featureNames: List[String], classNames: List[String]): String = {
      {
        val indentString = " " * indent
        s"${indentString}LeafNode(id=$id, classID=${classNames(classID)})"
      }
    }

    def toAst(features: List[BrboExpr], classes: List[Label], leafType: LeafType): BrboAst = {
      val className = classes(classID).name
      leafType match {
        case ResetLeaf(groupID) =>
          val reset = Reset(groupID.value)
          if (Classifier.resetLabelFromString(className)) {
            Reset(groupID.value)
          } else {
            Comment(reset.printToIR())
          }
        case UseLeaf(update) =>
          Classifier.useLabelFromString(className) match {
            case Classifier.AllGroups => throw new Exception
            case Classifier.NoneGroup => Comment(Use(None, update).printToIR())
            case Classifier.GeneralityTestGroup =>
              // For debugging purposes
              Use(Some(GeneralityTestGroup.value), update)
            case Classifier.GroupID(groupID) => Use(Some(groupID), update)
          }
        case _ => throw new Exception
      }
    }

    override def predicateCount(): Int = 0
  }

  case class InternalNode(override val id: Int, left: TreeNode, right: TreeNode, threshold: Float, featureID: Int) extends TreeNode(id) {
    override def toString: String = s"InterNode(id=$id, left=$left, right=$right, threshold=$threshold, featureID=$featureID)"

    def print(indent: Int, featureNames: List[String], classNames: List[String]): String = {
      val indentString = " " * indent
      val featureString = if (featureNames.nonEmpty) featureNames(featureID) else s"feature[$featureID]"
      val leftString = s"${left.print(indent + 2, featureNames, classNames)} (if $featureString <= $threshold)"
      val rightString = s"${right.print(indent + 2, featureNames, classNames)} (if $featureString > $threshold)"
      s"${indentString}InterNode(id=$id, threshold=$threshold, featureID=$featureID)\n$leftString\n$rightString"
    }

    def toAst(features: List[BrboExpr], classes: List[Label], leafType: LeafType): BrboAst = {
      val predicate = getPredicate(features)
      ITE(predicate, left.toAst(features, classes, leafType), right.toAst(features, classes, leafType))
    }

    def getPredicate(features: List[BrboExpr]): BrboExpr = {
      BrboExprUtils.lessThanOrEqualTo(features(featureID), Number(threshold.floor.toInt))
    }

    override def predicateCount(): Int = 1 + left.predicateCount() + right.predicateCount()
  }

  def parse(string: String): TreeClassifier = {
    val parsed = Json.parse(string)
    val leaves = parsed("leaves").as[List[LeafRawNode]]
    val nonLeaves = parsed("non_leaves").as[List[InternalRawNode]]
    val classes = parsed("classes").as[List[String]].map(name => Label(name))
    new TreeClassifier(root = buildTree(leaves, nonLeaves), labels = classes)
  }

  case class Label(name: String)

  class LeafType

  case class UseLeaf(update: BrboExpr) extends LeafType

  case class ResetLeaf(groupID: GroupID) extends LeafType

  class TreeClassifier(root: TreeNode, labels: List[Label]) {
    private val classNames = labels.map(l => l.name)

    def print(featureNames: List[String]): String = {
      s"Labels: $labels\nTree:\n${root.print(indent = 0, featureNames, classNames)}"
    }

    def toAst(features: List[BrboExpr], leafType: LeafType): BrboAst = root.toAst(features, labels, leafType)

    def predicateCount(): Int = root.predicateCount()

    def classify(store: Store,
                 evaluate: (BrboExpr, Store) => BrboValue,
                 features: List[BrboExpr]): Label =
      classifyInternal(root, store, evaluate, features)

    @tailrec
    private def classifyInternal(node: TreeNode,
                                 store: Store,
                                 evaluate: (BrboExpr, Store) => BrboValue,
                                 features: List[BrboExpr]): Label = {
      node match {
        case node: InternalNode =>
          evaluate(node.getPredicate(features), store) match {
            case Bool(b, _) =>
              if (b) classifyInternal(node.left, store, evaluate, features)
              else classifyInternal(node.right, store, evaluate, features)
            case _ => throw new Exception()
          }
        case LeafNode(_, classID) => labels(classID)
        case _ => throw new Exception
      }
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
