package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTraceAssociation, Trace}
import brbo.backend2.learning.Classifier.{GroupID, evaluateFunctionFromInterpreter}
import brbo.backend2.learning.ScriptRunner.{Algorithm, KMeans, Optics}
import brbo.backend2.learning.SegmentClustering._
import brbo.common.BrboType.INT
import brbo.common.MyLogger
import brbo.common.ast.{BrboExpr, Identifier}
import com.google.common.collect.Sets
import tech.tablesaw.api.{IntColumn, Table}

import java.util
import scala.collection.JavaConverters._

/**
 *
 * @param sumWeight     The weight of the sums of segment costs when deciding the similarity between two segments.
 * @param commandWeight The weight of the commands in segments when deciding the similarity between two segments.
 * @param debugMode     Whether to print more information.
 */
class SegmentClustering(sumWeight: Int, commandWeight: Int,
                        debugMode: Boolean) {
  private val logger = MyLogger.createLogger(classOf[SegmentClustering], debugMode)

  def decompose(trace: Trace, similarTraces: Iterable[Trace],
                interpreter: Interpreter, boundExpression: BrboExpr,
                debugMode: Boolean): TraceDecomposition = {
    val segmentClusteringAlgorithm = KMeans(clusters = Some(5))
    val decomposition = new TraceDecomposition(trace)
    var segmentLength = 1
    var excludeIndices: Set[Int] = Set()
    var madeProgress = true
    while (decomposition.getSize < trace.costTrace.nodes.size) {
      val clusters: List[List[Segment]] = clusterSimilarSegments(trace, segmentLength, segmentClusteringAlgorithm, excludeIndices)
      logger.info(s"Found ${clusters.size} segment clusters")
      var clusterId = 0
      while (madeProgress && clusterId < clusters.size) {
        madeProgress = false
        // Remove segments that contain indices that have been grouped
        val cluster = clusters(clusterId).filter({
          segment => segment.indices.toSet.intersect(excludeIndices).isEmpty
        })
        if (cluster.size > 1) {
          logger.info(s"Choose non-overlapping segments from cluster $clusterId")
          val nonOverlappingGroups = findNonOverlappingSegments(cluster)
          val generalizableGroups = chooseGeneralizableGroups(nonOverlappingGroups, similarTraces, interpreter, boundExpression)
          chooseGroup(generalizableGroups) match {
            case Some(chosenGroup) =>
              // Remove indices that have been grouped
              val chosenIndices = chosenGroup.indices
              excludeIndices = excludeIndices ++ chosenIndices
              madeProgress = true
            case None =>
          }
        }
        clusterId = clusterId + 1
      }
      // Prepare for the next iteration
      segmentLength = segmentLength + 1
    }
    // All remaining indices are put into a single segment for amortization
    val lastGroup = {
      val remainingIndices = trace.costTraceAssociation.indexMap.values.toSet.diff(excludeIndices).toList.sorted
      Group(List(Segment(remainingIndices)))
    }
    decomposition.addGroup(lastGroup)
    decomposition
  }

  def clusterSimilarSegments(trace: Trace, segmentLength: Int, algorithm: Algorithm, excludeIndices: Set[Int]): List[List[Segment]] = {
    val indices: util.Set[Int] = trace.costTraceAssociation.indexMap.values.toSet.diff(excludeIndices).asJava
    logger.info(s"Choose segments with sizes of $segmentLength")
    val segments: List[Segment] =
      Sets.combinations(indices, segmentLength)
        .asScala.map(subset => Segment(subset.asScala.toList.sorted))
        .toList

    val matrix = generateTrainingData(trace, segments, algorithm)
    val clusterLabels = Clustering.cluster(matrix, algorithm, debugMode) match {
      case Some(labels) => labels
      case None =>
        logger.error(s"Failed to find similar segments")
        return Nil
    }
    val labelMap: Map[Int, List[(Segment, Int)]] =
      segments.zip(clusterLabels).groupBy({ case (_, label) => label })

    // Ignore outliers
    val clusters: List[List[Segment]] =
      (labelMap - (-1)).values.map(list => list.map({ case (segment, _) => segment }).sortWith({
        case (segment1, segment2) => segment1.toString < segment2.toString
      })).toList.sortWith({
        case (list1, list2) => list1.toString() < list2.toString()
      })
    logger.info(s"Found ${clusters.size} segment clusters")
    clusters
  }

  // Given a list of potentially overlapping segments, choose sets of
  // non-overlapping segments
  def findNonOverlappingSegments(segments: List[Segment]): List[Group] = {
    val possibleGroups: List[Group] = segments.foldLeft(Nil: List[Group])({
      case (groups, segment) =>
        groups.find(group => group.addSegment(segment).isDefined) match {
          case Some(group) => group.addSegment(segment).get :: groups
          case None => Group(List(segment)) :: groups
        }
    })
    println(s"Found $possibleGroups possible groups (or non-overlapping sets of segments)")
    possibleGroups
  }

  def generateTrainingData(trace: Trace, segments: List[Segment], algorithm: Algorithm): List[List[Int]] = {
    val segmentToData = new SegmentToTrainingData(trace.costTraceAssociation, sumWeight, commandWeight)
    algorithm match {
      case Optics(_) =>
        segments.map({
          left =>
            segments.map({
              right => segmentToData.difference(left, right)
            })
        })
      case KMeans(_) =>
        segments.map(segment => List(segmentToData.toTrainingData(segment)))
      case _ => throw new Exception
    }
  }
}

object SegmentClustering {
  private val logger = MyLogger.createLogger(SegmentClustering.getClass, debugMode = false)

  private def chooseGeneralizableGroups(groups: List[Group], similarTraces: Iterable[Trace],
                                        interpreter: Interpreter, boundExpression: BrboExpr): List[Group] = {
    groups.filter({
      group =>
        similarTraces.forall({
          trace =>
            val tables = Classifier.generateTables(
              trace,
              Classifier.evaluateFunctionFromInterpreter(interpreter),
              Map(GroupID(421) -> group),
              features = List(Identifier("i", INT), Identifier("n", INT)),
              failIfCannotFindResetPlaceHolder = false
            )
            val classifierResults = tables.toProgramTables.runClassifier(debugMode = false)
            Classifier.satisfyBound(
              boundExpression,
              trace,
              evaluateFunctionFromInterpreter(interpreter),
              classifierResults,
              debugMode = false
            )
        })
    })
  }

  private def chooseGroup(groups: List[Group]): Option[Group] = {
    // TODO: Carefully choose a group
    if (groups.isEmpty)
      return None
    var maxGroupSize = 0
    var maxGroupIndex = -1
    groups.zipWithIndex.foreach({
      case (group, index) =>
        val size = group.size
        if (size > maxGroupSize) {
          maxGroupSize = size
          maxGroupIndex = index
        }
    })
    // logger.trace(s"The largest group has $maxGroupSize indices")
    Some(groups(maxGroupIndex))
  }

  // Indices in the list of costs
  case class Segment(indices: List[Int]) {
    assert(indices.nonEmpty && indices.sorted == indices,
      s"Indices must be non-empty and sorted: $indices")

    // Indices 1, 2, 4 overlap with 1, 5, 10, because 1 overlaps with 1
    // Indices 1, 2, 4 overlap with 3, 5, because interval [2,4] overlaps with [3,5]
    def overlap(other: Segment): Boolean = {
      if (indices.isEmpty || other.indices.isEmpty)
        true
      else {
        if (indices.head > other.indices.head)
          indices.head > other.indices.last
        else if (indices.head == other.indices.head)
          true
        else
          other.indices.head > indices.last
      }
    }

    def print(costTraceAssociation: CostTraceAssociation): String = {
      costTraceAssociation.costTrace(Some(indices)).map({
        case Interpreter.UseNode(use, cost) => s"${use.printToIR()} (cost=$cost)"
        case _ => throw new Exception
      }).mkString(", ")
    }

    def lessThanOrEqualTo(other: Segment): Boolean = indices.last <= other.indices.head
  }

  case class Group(segments: List[Segment]) {
    segments.zipWithIndex.foreach({
      case (segment, index) =>
        if (index < segments.length - 1) {
          assert(segment.indices.last < segments(index + 1).indices.head,
            s"Segments are sorted and must not overlap: $segment, ${segments(index + 1)}")
        }
    })
    val size: Int = segments.map(segment => segment.indices.size).sum

    val indices: List[Int] = segments.flatMap(segment => segment.indices)

    def addSegment(segment: Segment): Option[Group] = {
      if (segments.isEmpty)
        Some(Group(List(segment)))
      else {
        if (segments.last.overlap(segment))
          None // Cannot add the segment to this group
        else
          Some(Group(segments :+ segment))
      }
    }
  }

  class TraceDecomposition(trace: Trace) {
    private var groups: Map[GroupID, Group] = Map()
    private var size = 0

    def addGroup(group: Group): Unit = {
      val groupID = GroupID(groups.size)
      groups = groups + (groupID -> group)
      size = size + group.size
    }

    def getSize: Int = size

    def getGroups: Map[GroupID, Group] = groups

    def print(): String = printDecomposition(trace, groups)
  }

  class SegmentToTrainingData(costTraceAssociation: CostTraceAssociation, sumWeight: Int, commandWeight: Int) {
    def difference(left: Segment, right: Segment): Int = {
      sumDifference(left, right) + commandDifference(left, right)
    }

    def toTrainingData(segment: Segment): Int = {
      getSum(segment) * sumWeight
      // TODO: Output a weighted vector, where the first element is the sum and has the max weight
      //  and the remaining elements represent commands and have much smaller weights
    }

    private def sumDifference(left: Segment, right: Segment): Int = {
      Math.abs(toTrainingData(left) - toTrainingData(right))
    }

    private def getSum(segment: Segment): Int = costTraceAssociation.costSumAtIndices(segment.indices)

    private def commandDifference(left: Segment, right: Segment): Int = {
      val leftCommands = left.indices.map(index => costTraceAssociation.ghostCommandAtIndex(index)).toSet
      val rightCommands = right.indices.map(index => costTraceAssociation.ghostCommandAtIndex(index)).toSet
      // A large difference in the commands means the group (that includes the two segments)
      // contains (too) many different commands, which may be too complicated
      leftCommands.diff(rightCommands).size * commandWeight
    }
  }

  def printDecomposition(trace: Trace, groups: Map[GroupID, Group]): String = {
    val table: Table = trace.toTable
    val sortedMap = groups.toList.sortWith({
      case ((id1, _), (id2, _)) => id1.print() < id2.print()
    })
    sortedMap.foreach({
      case (groupID, group) =>
        val column = IntColumn.create(groupID.print())
        Range(0, table.rowCount()).foreach({
          index =>
            group.segments.indexWhere(segment => segment.indices.contains(index)) match {
              case -1 => column.appendMissing()
              case n => column.append(n)
            }
        })
        table.addColumns(column)
    })
    table.printAll()
  }
}