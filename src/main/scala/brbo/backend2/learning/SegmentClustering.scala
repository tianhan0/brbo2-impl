package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTraceAssociation, Trace}
import brbo.backend2.learning.Classifier._
import brbo.backend2.learning.ScriptRunner._
import brbo.backend2.learning.SegmentClustering._
import brbo.common.BrboType.INT
import brbo.common.MyLogger
import brbo.common.ast.Identifier
import com.google.common.collect.Sets
import tech.tablesaw.api.IntColumn

import java.util
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 *
 * @param sumWeight     The weight of the sums of segment costs when deciding the similarity between two segments.
 * @param commandWeight The weight of the commands in segments when deciding the similarity between two segments.
 * @param debugMode     Whether to print more information.
 */
class SegmentClustering(sumWeight: Int, commandWeight: Int,
                        debugMode: Boolean, val algorithm: Algorithm) {
  private val logger = MyLogger.createLogger(classOf[SegmentClustering], debugMode)

  def decompose(trace: Trace, similarTraces: Iterable[Trace], interpreter: Interpreter): TraceDecomposition = {
    val decomposition = new TraceDecomposition(trace)
    var segmentLength = 1
    var excludeIndices: Set[Int] = Set()
    var madeProgress = true
    while (madeProgress && decomposition.getSize < trace.costTrace.nodes.size) {
      madeProgress = false
      logger.info(s"Cluster segments with length $segmentLength")
      val clusters: List[List[Segment]] = clusterSimilarSegments(trace, segmentLength, excludeIndices)
      var clusterId = 0
      while (clusterId < clusters.size) {
        // Remove segments that contain indices that have been grouped
        val cluster = clusters(clusterId).filter({
          segment => segment.indices.toSet.intersect(excludeIndices).isEmpty
        })
        logger.info(s"Visit $clusterId-th cluster: ${printSegments(cluster)}")
        if (cluster.size > 1) {
          logger.info(s"Choose non-overlapping segments from cluster $clusterId")
          val nonOverlappingGroups = findNonOverlappingSegments(cluster)
          val generalizableGroups = chooseGeneralizableGroups(
            nonOverlappingGroups,
            testTrace = trace,
            similarTraces,
            interpreter,
            sampleKTraces = Some(2)
          )
          chooseGroup(generalizableGroups) match {
            case Some(chosenGroup) =>
              decomposition.addGroup(chosenGroup)
              // Remove indices that have been grouped
              val chosenIndices = chosenGroup.indices
              logger.info(s"Chosen group: ${printSegments(chosenGroup.segments)}")
              logger.traceOrError(s"Chosen group on trace:\n${printDecomposition(trace, Map(PrintGroup -> chosenGroup))}")
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

    val remainingIndices = trace.costTraceAssociation.indexMap.keys.toSet.diff(excludeIndices).toList.sorted
    if (remainingIndices.nonEmpty) {
      val lastGroup = Group(List(Segment(remainingIndices)))
      decomposition.addGroup(lastGroup)
    }
    decomposition
  }

  def clusterSimilarSegments(trace: Trace, segmentLength: Int, excludeIndices: Set[Int]): List[List[Segment]] = {
    val indices: util.Set[Int] = trace.costTraceAssociation.indexMap.keys.toSet.diff(excludeIndices).asJava
    logger.traceOrError(s"Choose segments with sizes of $segmentLength among trace node indices $indices")
    if (segmentLength > indices.size()) return Nil
    val segments: List[Segment] =
      Sets.combinations(indices, segmentLength)
        .asScala.map(subset => Segment(subset.asScala.toList.sorted))
        .toList
    clusterSimilarSegments(trace, segments)
  }

  // Given a list of potentially overlapping segments, choose sets of
  // non-overlapping segments
  def findNonOverlappingSegments(segments: List[Segment]): List[Group] = {
    val possibleGroups: List[Group] =
      segments.sortWith({ case (s1, s2) => s1.lessThan(s2) }).foldLeft(Nil: List[Group])({
        case (groups, segment) =>
          var createNewGroup = true
          val newGroups = groups.map({
            group =>
              group.addSegment(segment) match {
                case Some(newGroup) =>
                  createNewGroup = false
                  newGroup
                case None => group
              }
          })
          if (createNewGroup) Group(List(segment)) :: newGroups
          else newGroups
      })
    logger.info(s"Found ${possibleGroups.size} possible groups (or non-overlapping sets of segments)")
    possibleGroups
  }

  def generateTrainingData(trace: Trace, segments: List[Segment]): List[List[Int]] = {
    val segmentToData = new SegmentToTrainingData(trace.costTraceAssociation, sumWeight, commandWeight)
    algorithm match {
      case Optics(_, Precomputed) =>
        segments.map({
          left =>
            segments.map({ right => segmentToData.difference(left, right) })
        })
      case KMeans(_) | Optics(_, Euclidean) =>
        segments.map(segment => List(segmentToData.toTrainingData(segment)))
      case _ => throw new Exception
    }
  }

  def clusterSimilarSegments(trace: Trace, segments: List[Segment]): List[List[Segment]] = {
    val matrix = generateTrainingData(trace, segments)
    val clusterLabels = Clustering.cluster(matrix, algorithm, debugMode) match {
      case Some(labels) => labels
      case None =>
        logger.fatal(s"Failed to find similar segments")
        return Nil
    }
    val labelMap: Map[Int, List[(Segment, Int)]] = {
      val labelMap = segments.zip(clusterLabels).groupBy({ case (_, label) => label })
      algorithm match {
        case _: Optics => labelMap - (-1) // Ignore outliers
        case _ => labelMap
      }
    }

    val clusters: List[List[Segment]] =
      labelMap.values.map(list => list.map({ case (segment, _) => segment }).sortWith({
        case (segment1, segment2) => segment1.toString < segment2.toString
      })).toList.sortWith({
        case (list1, list2) => list1.toString() < list2.toString()
      })
    logger.info(s"Found ${clusters.size} segment clusters")
    clusters
  }

  def chooseGeneralizableGroups(groups: List[Group],
                                testTrace: Trace,
                                similarTraces: Iterable[Trace],
                                interpreter: Interpreter,
                                sampleKTraces: Option[Int]): List[Group] = {
    val sampledSimilarTraces = {
      sampleKTraces match {
        case Some(sampleKTraces) =>
          similarTraces.zipWithIndex.groupBy({ case (_, index) => index % sampleKTraces }).map({
            case (_, list) => list.last
          })
        case None => similarTraces.zipWithIndex
      }
    }

    val futures = Future.traverse(groups.zipWithIndex)({
      case (group, groupIndex) =>
        Future {
          val classifierResults =
            try {
              val tables = Classifier.generateTables(
                testTrace,
                Classifier.evaluateFunctionFromInterpreter(interpreter),
                Map(GeneralityTestGroup -> group),
                features = List(Identifier("i", INT), Identifier("n", INT)),
                failIfCannotFindResetPlaceHolder = true
              )
              // logger.traceOrError(s"Generated table: ${tables.print()}")
              val classifierResults = tables.toProgramTables.generateClassifiers(debugMode)
              Some(classifierResults)
            } catch {
              case TableGenerationError(message) =>
                logger.info(message)
                None
            }

          sampledSimilarTraces.forall({
            case (trace, traceIndex) =>
              logger.info(s"Test the generality of $groupIndex-th group on $traceIndex-th trace")
              logger.info(s"Test group: ${printSegments(group.segments)}")
              classifierResults match {
                case Some(classifierResults) =>
                  val applicationResult = Classifier.applyClassifiers(
                    boundExpression = None,
                    trace,
                    evaluateFunctionFromInterpreter(interpreter),
                    classifierResults,
                    debugMode
                  )
                  val areSimilar = applicationResult.areActualSegmentCostsSimilar(this)
                  logger.info(s"Tested the generality of $groupIndex-th group on $traceIndex-th trace: $areSimilar")
                  logger.info(s"Tested group on trace:\n${trace.toTable(printStores = false, onlyGhostCommand = true)._1.printAll()}")
                  areSimilar
                case None => false
              }
          })
        }
    })
    val result = Await.result(futures, Duration.Inf)
    groups.zip(result).flatMap({
      case (group, areSimilar) => if (areSimilar) Some(group) else None
    })
  }
}

object SegmentClustering {
  private val logger = MyLogger.createLogger(SegmentClustering.getClass, debugMode = false)

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
    assert(indices.sorted == indices, s"Indices must be sorted: $indices")
    val head: Option[Int] = indices.headOption
    val last: Option[Int] = indices.lastOption

    // Indices 1, 2, 4 overlap with 1, 5, 10, because 1 overlaps with 1
    // Indices 1, 2, 4 overlap with 3, 5, because interval [2,4] overlaps with [3,5]
    def notOverlap(other: Segment): Boolean = {
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

    def print(trace: Trace): String = {
      trace.costTraceAssociation.costTrace(Some(indices)).map({
        case Interpreter.UseNode(use, cost) => s"${use.printToIR()} (cost=$cost)"
        case _ => throw new Exception
      }).mkString(", ")
    }

    def printAsSet(): String = s"{${indices.mkString(",")}}"

    def lessThan(other: Segment): Boolean = {
      (indices.isEmpty, other.indices.isEmpty) match {
        case (true, true) => true
        case (true, false) => true
        case (false, true) => false
        case (false, false) => indices.last < other.indices.head
      }
    }

    def includes(other: Segment): Boolean = other.indices.toSet.subsetOf(this.indices.toSet)

    def add(index: Int): Segment = Segment(indices :+ index)
  }

  case class Group(segments: List[Segment]) {
    segments.zipWithIndex.foreach({
      case (segment, index) =>
        if (index < segments.length - 1) {
          val current = segment
          val next = segments(index + 1)
          assert(current.notOverlap(next) && current.lessThan(next),
            s"Segments are sorted and must not overlap: $current, $next")
        }
    })
    val size: Int = segments.map(segment => segment.indices.size).sum

    val indices: List[Int] = segments.flatMap(segment => segment.indices)

    val head: Option[Int] = {
      if (segments.isEmpty) None
      else {
        segments.indexWhere(s => s.head.isDefined) match {
          case -1 => None
          case index => segments(index).head
        }
      }
    }

    val last: Option[Int] = {
      if (segments.isEmpty) None
      else {
        segments.lastIndexWhere(s => s.last.isDefined) match {
          case -1 => None
          case index => segments(index).last
        }
      }
    }

    def addSegment(segment: Segment): Option[Group] = {
      if (segments.isEmpty)
        Some(Group(List(segment)))
      else {
        if (!segments.last.notOverlap(segment))
          None // Cannot add the segment to this group
        else
          Some(Group(segments :+ segment))
      }
    }

    def lessThanOrEqualTo(other: Group): Boolean = {
      (this.head, other.head) match {
        case (Some(thisHead), Some(otherHead)) => thisHead <= otherHead
        case (Some(_), None) => false
        case (None, Some(_)) => true
        case (None, None) => true
      }
    }

    def removeEmptySegments(): Group = Group(segments.filter(s => s.indices.nonEmpty))

    def includes(other: Group): Boolean = {
      val thisRemovedEmptySegments = removeEmptySegments()
      val otherRemovedEmptySegments = other.removeEmptySegments()
      otherRemovedEmptySegments.segments.forall({
        otherSegment =>
          thisRemovedEmptySegments.segments.exists(thisSegment => thisSegment.includes(otherSegment))
      })
    }

    def print(trace: Trace): String = {
      removeEmptySegments().segments.map({ segment => segment.print(trace) }).mkString("; ")
    }
  }

  def printGroups(groups: Iterable[Group], trace: Trace): String = {
    groups.map(g => g.print(trace)).mkString("\n")
  }

  def printSegments(segments: Iterable[Segment]): String = {
    segments.map(segment => segment.printAsSet()).toList.sorted.mkString(", ")
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
    val (table, indexMap) = trace.toTable(printStores = false, onlyGhostCommand = true)
    val sortedMap = groups.toList.sortWith({
      case ((id1, _), (id2, _)) => id1.print() < id2.print()
    })
    sortedMap.foreach({
      case (groupID, group) =>
        val column = IntColumn.create(s"SegmentIDs in ${groupID.print()}")
        Range(0, table.rowCount()).foreach({
          index =>
            val originalIndex = indexMap(index)
            group.segments.indexWhere(segment => segment.indices.contains(originalIndex)) match {
              case -1 => column.appendMissing()
              case n => column.append(n)
            }
        })
        table.addColumns(column)
    })
    table.printAll()
  }
}