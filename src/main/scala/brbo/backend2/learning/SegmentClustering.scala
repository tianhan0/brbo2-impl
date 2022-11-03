package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, CostTraceAssociation, Trace}
import brbo.backend2.learning.Clustering.{Algorithm, KMeans, Optics}
import brbo.backend2.learning.SegmentClustering._
import brbo.common.MyLogger
import com.google.common.collect.Sets

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

  def cluster(trace: Trace, similarTraces: Iterable[Trace],
              bound: Int, debugMode: Boolean): Unit = {
  }

  def decompose(trace: Trace, similarTraces: Iterable[Trace],
                bound: Int): Decomposition = {
    val decomposition = new Decomposition(trace)
    val numberOfCosts = trace.costTrace.nodes.size
    var segmentLength = 1
    while (decomposition.getSize < numberOfCosts) {
      val clusters: List[List[Segment]] = clusterSimilarSegments(trace, segmentLength, KMeans(Some(5)))
      logger.info(s"Found ${clusters.size} segment clusters")

      var clusterId = 0
      while (clusterId < clusters.size) {
        val segments = clusters(clusterId)
        logger.info(s"Choose non-overlapping segments from cluster $clusterId")
        val nonOverlappingGroups = findNonOverlappingSegments(segments)
        val chosenGroup = chooseMaxGroup(nonOverlappingGroups)
        val chosenIndices = chosenGroup.indices
        // TODO: Remove segments that contain any of the chosen indices
        clusterId = clusterId + 1
      }
      // Prepare for the next iteration
      // TODO: Remove indices that have been grouped
      segmentLength = segmentLength + 1
    }
    // TODO: All remaining indices will be put into a single segment for amortization
    decomposition
  }

  def clusterSimilarSegments(trace: Trace, segmentLength: Int, algorithm: Algorithm): List[List[Segment]] = {
    val indices: util.Set[Int] = trace.costTraceAssociation.indexMap.values.toSet.asJava
    logger.info(s"Choose segments with sizes of $segmentLength")
    val segments: List[Segment] =
      Sets.combinations(indices, segmentLength)
        .asScala.map(subset => Segment(subset.asScala.toList.sorted))
        .toList

    val matrix = generateInputData(trace, segments, algorithm)
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

  def generateInputData(trace: Trace, segments: List[Segment], algorithm: Algorithm): List[List[Int]] = {
    val segmentToData = new SegmentToData(trace.costTraceAssociation, sumWeight, commandWeight)
    algorithm match {
      case Optics(_) =>
        segments.map({
          left =>
            segments.map({
              right => segmentToData.difference(left, right)
            })
        })
      case KMeans(_) =>
        segments.map(segment => List(segmentToData.toData(segment)))
      case _ => throw new Exception
    }
  }
}

object SegmentClustering {
  private def chooseMaxGroup(groups: List[Group]): Group = {
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
    println(s"The largest group has $maxGroupSize indices")
    groups(maxGroupIndex)
  }

  case class TraceDecomposition(trace: Trace, decomposition: Decomposition)

  // Indices in the list of costs. Indices are sorted
  case class Segment(indices: List[Int]) {
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
  }

  // Segments are sorted and must not overlap
  case class Group(segments: List[Segment]) {
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

  class Decomposition(trace: Trace) {
    private var groups: Set[Group] = Set()
    private var size = 0

    def addGroup(group: Group): Unit = {
      groups = groups + group
      size = size + group.size
    }

    def getSize: Int = size

    def getGroups: Set[Group] = groups
  }

  class SegmentToData(costTraceAssociation: CostTraceAssociation, sumWeight: Int, commandWeight: Int) {
    def difference(left: Segment, right: Segment): Int = {
      sumDifference(left, right) + commandDifference(left, right)
    }

    def toData(segment: Segment): Int = {
      getSum(segment) * sumWeight
      // TODO: Output a weighted vector, where the first element is the sum and has the max weight
      //  and the remaining elements represent commands and have much smaller weights
    }

    private def sumDifference(left: Segment, right: Segment): Int = {
      Math.abs(toData(left) - toData(right))
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
}