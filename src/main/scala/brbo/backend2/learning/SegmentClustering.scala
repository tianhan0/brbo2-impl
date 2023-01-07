package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.Classifier._
import brbo.backend2.learning.ScriptRunner._
import brbo.backend2.learning.SegmentClustering._
import brbo.common.ast._
import brbo.common.cfg.ControlFlowGraph
import brbo.common.{BrboType, MyLogger}
import com.google.common.collect.Sets
import tech.tablesaw.api.IntColumn

import java.util
import java.util.concurrent.Executors
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 *
 * @param sumWeight     The weight of the sums of segment costs when deciding the similarity between two segments.
 * @param commandWeight The weight of the commands in segments when deciding the similarity between two segments.
 * @param debugMode     Whether to print more information.
 */
class SegmentClustering(sumWeight: Int,
                        commandWeight: Int,
                        debugMode: Boolean,
                        val algorithm: Algorithm,
                        threads: Int) {
  private val logger = MyLogger.createLogger(classOf[SegmentClustering], debugMode)
  private val MAX_SEGMENT_LENGTH = 3
  private val executionContextExecutor = {
    val executorService = Executors.newFixedThreadPool(threads)
    ExecutionContext.fromExecutor(executorService)
  }

  def decompose(trace: Trace,
                similarTraces: Iterable[Trace],
                interpreter: Interpreter,
                features: List[Identifier]): TraceDecomposition = {
    val decomposition = new TraceDecomposition(trace)
    var segmentLength = 1
    var excludeIndices: Set[Int] = Set()
    var remainingIndices: Set[Int] = trace.costTraceAssociation.indexMap.keys.toSet
    while (decomposition.getSize < trace.costTrace.nodes.size
      && segmentLength <= remainingIndices.size
      && segmentLength <= MAX_SEGMENT_LENGTH) {
      logger.info("-" * 80)
      logger.info(s"Cluster segments with length $segmentLength")
      val candidateSegments: List[Segment] = generateAndFilterSegments(trace, segmentLength, excludeIndices)
      val clusters: List[List[Segment]] = clusterSimilarSegments(trace, candidateSegments)
      var clusterId = 0
      while (clusterId < clusters.size) {
        // Remove segments that contain indices that have been grouped
        val cluster = clusters(clusterId).filter({
          segment => segment.indices.toSet.intersect(excludeIndices).isEmpty
        })
        logger.info(s"Visit $clusterId-th cluster (segment length: $segmentLength)")
        if (cluster.size > 1) {
          logger.info(s"Choose non-overlapping segments from $clusterId-th cluster")
          val nonOverlappingGroups: List[Group] = findNonOverlappingSegments(cluster)
          // Assume the selected traces ensure the selected groups are correct.
          // However, we still need to choose a generalization of the group, by choosing the reset locations.
          /* val generalizableGroups = chooseGeneralizableGroups(
            nonOverlappingGroups,
            testTrace = trace,
            similarTraces,
            interpreter,
            features = features,
            sampleKTraces = None
          )*/
          chooseGroup(nonOverlappingGroups) match {
            case Some(chosenGroup) =>
              decomposition.addGroup(chosenGroup)
              // Remove indices that have been grouped
              val chosenIndices = chosenGroup.indices
              logger.info(s"Chosen group: ${printSegments(chosenGroup.segments)} " +
                s"on trace:\n${printDecomposition(trace, Map(PrintGroup -> chosenGroup))}")
              excludeIndices = excludeIndices ++ chosenIndices
              remainingIndices = remainingIndices -- chosenIndices
            case None =>
          }
        }
        clusterId = clusterId + 1
      }
      // Prepare for the next iteration
      segmentLength = segmentLength + 1
    }
    // All remaining indices are put into a single segment for amortization

    if (remainingIndices.nonEmpty) {
      val lastGroup = Group(List(Segment(remainingIndices.toList.sorted)))
      decomposition.addGroup(lastGroup)
    }
    decomposition
  }

  def generateSegments(trace: Trace, segmentLength: Int, excludeIndices: Set[Int]): List[Segment] = {
    val indices: util.Set[Int] = trace.costTraceAssociation.indexMap.keys.toSet.diff(excludeIndices).asJava
    logger.traceOrError(s"Choose segments with sizes of $segmentLength among trace node indices $indices")
    if (segmentLength > indices.size()) return Nil
    Sets.combinations(indices, segmentLength).asScala
      .map(subset => Segment(subset.asScala.toList.sorted))
      .toList
  }

  // Remove a segment if there exists a longer and subsuming segment whose similarity is better
  // The goal is to prioritize segments that are more similar to some input, such that the relation
  // between a segment's cost and an input can be expressed with simpler invariants
  def generateAndFilterSegments(trace: Trace, segmentLength: Int, excludeIndices: Set[Int]): List[Segment] = {
    val segments = generateSegments(trace, segmentLength, excludeIndices)
    val longerSegments: Seq[Segment] = Range.inclusive(segmentLength, MAX_SEGMENT_LENGTH).flatMap({
      segmentLength => generateSegments(trace, segmentLength, excludeIndices)
    })
    val similarities = longerSegments.map(segment => (segment, segmentSimilarityWithInputs(segment, trace))).toMap
    segments.filter({
      segment =>
        val indices = segment.indices.toSet
        val similarity = segmentSimilarityWithInputs(segment, trace)
        val betterSimilarity = longerSegments.find({
          longerSegment =>
            if (indices.subsetOf(longerSegment.indices.toSet)) {
              // A longer and subsuming segment must be less similar
              similarities(longerSegment) > similarity
            } else {
              // This not a longer and subsuming segment
              false
            }
        })
        if (betterSimilarity.nonEmpty)
          logger.info(s"Remove segment ${segment.printAsSet} because there exists a longer and subsuming segment ${betterSimilarity.get.printAsSet} " +
            s"whose similarity with inputs is better")
        betterSimilarity.isEmpty
    })
  }

  private def segmentSimilarityWithInputs(segment: Segment, trace: Trace): Double = {
    val inputValues: Iterable[Value] = trace.inputs.flatMap({
      case (_, Number(n, _)) => Some(IntegerValue(n))
      case (_, BrboArray(values, BrboType.INT, _)) => Some(ArrayValue(values.map({ case Number(n, _) => n })))
      case _ => None
    })
    val segmentCosts = segment.getCosts(trace)
    segmentCosts.size match {
      case 0 => Value.MIN_SIMILARITY // We do not prefer empty segments
      case 1 =>
        // If a segment has 1 cost, then this segment may be similar to an integer-typed input, or an array-typed input (with a single element)
        val possibleValues = List(IntegerValue(segmentCosts.head), ArrayValue(segmentCosts))
        val similarities: List[Double] = possibleValues.flatMap({
          possibleValue =>
            Value.similarity(
              value = possibleValue,
              values = inputValues,
              maxSimilarity = Value.MAX_SIMILARITY,
              minSimilarity = Value.MIN_SIMILARITY
            )
        })
        similarities.max
      case _ =>
        val similarities = Value.similarity(
          value = ArrayValue(segmentCosts),
          values = inputValues,
          maxSimilarity = Value.MAX_SIMILARITY,
          minSimilarity = Value.MIN_SIMILARITY
        )
        similarities.max
    }
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
    val segmentToData = new SegmentToTrainingData(trace, sumWeight, commandWeight)
    algorithm match {
      case Optics(_, Precomputed) =>
        segments.map({
          left => segments.map({ right => segmentToData.difference(left, right) })
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
    logger.info(s"Clustered similar segments: Found ${clusters.size} segment clusters")
    clusters
  }

  def chooseGeneralizableGroups(groups: List[Group],
                                testTrace: Trace,
                                similarTraces: Iterable[Trace],
                                interpreter: Interpreter,
                                features: List[Identifier],
                                sampleKTraces: Option[Int]): List[Group] = {
    val lengthMap = similarTraces.map(t => (t, t.nodes.size)).toMap
    val distinctLengths = lengthMap.values.toList.sorted.distinct
    logger.infoOrError(s"Similar traces have the following (distinct) lengths: $distinctLengths")
    val sampledSimilarTraces = {
      sampleKTraces match {
        case Some(sampleKTraces) =>
          val chosenLengths: List[Int] =
            if (distinctLengths.size <= sampleKTraces) distinctLengths
            else {
              // logger.info(s"${distinctLengths.zipWithIndex.groupBy({ case (_, index) => index % sampleKTraces })}")
              /*distinctLengths.zipWithIndex.groupBy({ case (_, index) => index % sampleKTraces }).map({
                case (_, tuples) => tuples.head._1
              }).toList*/
              // Choose the longest traces to test generality
              distinctLengths.slice(distinctLengths.size - sampleKTraces, distinctLengths.size)
            }
          logger.infoOrError(s"Choose traces with the following lengths: $chosenLengths")
          val traces = chosenLengths.map({
            length => lengthMap.find({ case (_, i) => i == length }).get
          }).map({ case (trace, _) => trace })
          traces.zipWithIndex
        case None => similarTraces.zipWithIndex
      }
    }

    val possibleFeatures: List[List[Identifier]] = List(features)
    // The features are a variable (as opposed to all variables)
    // val possibleFeatures: List[List[Identifier]] = features.map(identifier => List(identifier))
    val result = groups.zipWithIndex.map({
      case (group, groupIndex) if group.segments.size > 1 =>
        val printGroup = printSegments(group.segments)
        /* TODO: Select reset locations for the group, such that the generalization works for other traces
        val resetPlaceHolderIndices: List[Set[Int]] = ResetPlaceHolderFinder.indices(
          trace = testTrace,
          group = group,
          throwIfNoResetPlaceHolder = true
        )
        val groupsWithResets = resetPlaceHolderIndices.map({
          resetIndices => GroupWithResets(group.segments, resetIndices)
        })*/
        // If there exist a set of features under which the given group is generalizable to all traces
        val futures = Future.traverse(possibleFeatures)({
          features =>
            Future {
              val printFeatures = s"features: ${features.map(identifier => identifier.printToIR()).mkString(",")}"
              val classifierResults =
                try {
                  logger.infoOrError(s"Train a classifier for ${groupIndex + 1}-th group (among ${groups.size}): " +
                    s"$printGroup ($printFeatures)")
                  val groupsMap = Map[GroupID, Group](GeneralityTestGroup -> group)
                  val resetPlaceHolderIndices: Map[GroupID, Set[Int]] = ResetPlaceHolderFinder.indices(
                    trace = testTrace,
                    groups = groupsMap,
                    controlFlowGraph = ControlFlowGraph.toControlFlowGraph(interpreter.brboProgram),
                    throwIfNoResetPlaceHolder = true
                  )
                  val tables = Classifier.generateTables(
                    testTrace,
                    Classifier.evaluateFromInterpreter(interpreter),
                    groupsMap,
                    features = features,
                    resetPlaceHolderIndices = resetPlaceHolderIndices,
                  )
                  val programTables = tables.toProgramTables
                  logger.infoOrError(s"Generated training data")
                  // logger.traceOrError(s"${programTables.print()}")
                  val classifierResults = programTables.generateClassifiers(debugMode)
                  Some(classifierResults)
                } catch {
                  case TableGenerationError(message) =>
                    logger.infoOrError(s"Failed to train a classifier: $message")
                    None
                }

              sampledSimilarTraces.forall({
                case (trace, traceIndex) =>
                  val logging = s"Test the generality of ${groupIndex + 1}-th group (among ${groups.size}) " +
                    s"on ${traceIndex + 1}-th trace (among ${sampledSimilarTraces.size}) " +
                    s"(length: ${trace.length}) (group: $printGroup) ($printFeatures)"
                  logger.infoOrError(logging)
                  classifierResults match {
                    case Some(classifierResults) =>
                      val applicationResult = Classifier.applyClassifiers(
                        boundExpression = None,
                        trace,
                        evaluateFromInterpreter(interpreter),
                        classifierResults,
                        debugMode
                      )
                      val stringBuilder = new mutable.StringBuilder
                      val areSimilar = applicationResult.areActualSegmentCostsSimilar(
                        segmentClustering = this,
                        stringBuilder = stringBuilder,

                        /**
                         * TODO: Empty segments are vacuously similar to each other, but if a grouping is truly
                         * generalizable to a similar trace, it should not result in all empty segments
                         */
                        diffIfNoSegmentAfterDecomposition = true,

                        /**
                         * TODO: This is a hack. We want to avoid guarding resets with predicates (which may cause the
                         * verifier to fail when the resets are in loops). Hence, when testing the generality of a
                         * trace decomposition, if the last segment of a group is empty, we want to consider
                         * this decomposition as generalizable. Hence, we ignore this segment when re-clustering the segments.
                         */
                        removeLastEmptySegmentAfterDecomposition = true,
                      )
                      stringBuilder.append(Classifier.printTransformation(classifierResults.toTransformation) + "\n")
                      if (areSimilar) {
                        // Print details for traces whose decomposition yield segments with similar costs
                        stringBuilder.append(s"Decomposed trace:${applicationResult.printDecomposedTrace}\n")
                        stringBuilder.append(s"$logging: $areSimilar\n")
                        logger.infoOrError(s"Re-clustered segments are the same${stringBuilder.toString()}")
                      } else {
                        // stringBuilder.append(s"Decomposed trace:${applicationResult.printDecomposedTrace}\n")
                        logger.infoOrError(s"Re-clustered segments are different${stringBuilder.toString()}")
                      }
                      // logger.traceOrError(s"Tested group on trace:\n${trace.toTable(printStores = false, onlyGhostCommand = true)._1.printAll()}")
                      areSimilar
                    case None =>
                      logger.infoOrError(s"Cannot test the generality of ${groupIndex + 1}-th group on $traceIndex-th trace. " +
                        s"No classifier for $printGroup ($printFeatures)")
                      false
                  }
              })
            }(executionContextExecutor)
        })(implicitly, executionContextExecutor)
        Await.result(futures, Duration.Inf).contains(true)
      case _ =>
        // If a group contains a single segment, then we cannot witness (at least) two segments that have similar costs
        false
    })
    val generalizableGroups = groups.zip(result).flatMap({
      case (group, areSimilar) => if (areSimilar) Some(group) else None
    })
    logger.info(s"Generalizable groups:\n${generalizableGroups.map({ group => printSegments(group.segments) }).mkString("  \n")}")
    generalizableGroups
  }
}

object SegmentClustering {
  private val logger = MyLogger.createLogger(SegmentClustering.getClass, debugMode = false)
  val THREADS: Int = Runtime.getRuntime.availableProcessors

  abstract class Value

  case class IntegerValue(n: Int) extends Value

  case class ArrayValue(array: List[Int]) extends Value

  object Value {
    val MAX_SIMILARITY = 100
    val MIN_SIMILARITY = 0

    private def similarity(value: Value, against: Value, maxSimilarity: Int, minSimilarity: Int): Double = {
      (value, against) match {
        case (IntegerValue(n1), IntegerValue(n2)) =>
          if (n1 == n2) maxSimilarity else minSimilarity
        case (ArrayValue(array1), ArrayValue(array2)) =>
          val set1 = array1.toSet
          val set2 = array2.toSet
          val intersection = set1.intersect(set2)
          val union = set1.union(set2)
          val percentage = intersection.size.toDouble / union.size
          minSimilarity + (maxSimilarity - minSimilarity) * percentage
        case (array: ArrayValue, integer: IntegerValue) =>
          similarity(integer, array, maxSimilarity = maxSimilarity, minSimilarity = minSimilarity)
        case (integer: IntegerValue, array: ArrayValue) =>
          similarity(integer, array, maxSimilarity = maxSimilarity, minSimilarity = minSimilarity)
        case _ => throw new Exception
      }
    }

    private def similarity(integerValue: IntegerValue, arrayValue: ArrayValue, maxSimilarity: Int, minSimilarity: Int): Double = {
      Value.similarity(
        value = ArrayValue(List(integerValue.n)),
        values = List(arrayValue),
        maxSimilarity = Value.MAX_SIMILARITY,
        minSimilarity = Value.MIN_SIMILARITY
      ).head
    }

    // Compute a value's similarity against all values
    def similarity(value: Value, values: Iterable[Value], maxSimilarity: Int, minSimilarity: Int): Iterable[Double] = {
      values.map({
        inputValue =>
          Value.similarity(
            value = value,
            against = inputValue,
            maxSimilarity = maxSimilarity,
            minSimilarity = minSimilarity
          )
      })
    }

    def belowThreshold(similarities: Iterable[Double], threshold: Double): Boolean =
      similarities.forall(similarity => similarity <= threshold)
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
    assert(indices.sorted == indices, s"Indices must be sorted: $indices")
    val head: Option[Int] = indices.headOption
    val last: Option[Int] = indices.lastOption

    val isEmpty: Boolean = indices.isEmpty

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

    lazy val printAsSet: String = s"{${indices.mkString(",")}}"

    def lessThan(other: Segment): Boolean = {
      (this.isEmpty, other.isEmpty) match {
        case (true, true) => false
        case (true, false) => true
        case (false, true) => false
        case (false, false) => indices.head < other.indices.head
      }
    }

    def sameAs(other: Segment): Boolean = indices == other.indices

    def add(index: Int): Segment = Segment(indices :+ index)

    def getCosts(trace: Trace): List[Int] = trace.costTraceAssociation.costsAtIndices(indices)

    def getCostSum(trace: Trace): Int = trace.costTraceAssociation.costSumAtIndices(indices)
  }

  abstract class AbstractGroup(val segments: List[Segment]) {
    val nonEmptySegments: List[Segment] = segments.filterNot(s => s.isEmpty)
    nonEmptySegments.zipWithIndex.foreach({
      case (segment, index) =>
        if (index < nonEmptySegments.length - 1) {
          val current = segment
          val next = nonEmptySegments(index + 1)
          assert(current.notOverlap(next) && current.lessThan(next),
            s"Segments are sorted and must not overlap: $current, $next")
        }
    })
    val numberOfEmptySegments: Int = segments.size - nonEmptySegments.size
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
        if (!segments.last.notOverlap(segment) || !segments.last.lessThan(segment))
          None // Cannot add the segment to this group
        else
          Some(Group(segments :+ segment))
      }
    }

    def sameAs(other: Group): Boolean = {
      if (numberOfEmptySegments != other.numberOfEmptySegments)
        return false
      if (nonEmptySegments.size != other.nonEmptySegments.size)
        return false
      nonEmptySegments.zip(other.nonEmptySegments).forall({
        case (segment, otherSegment) => segment.sameAs(otherSegment)
      })
    }

    def print(trace: Trace): String = {
      nonEmptySegments.map({ segment => segment.print(trace) }).mkString("; ")
    }

    def getCommands(trace: Trace): List[Command] = {
      trace.nodes.zipWithIndex.foldLeft(Nil: List[Command])({
        case (soFar, (node, index)) =>
          if (indices.contains(index)) {
            node.lastTransition match {
              case Some(transition) => transition.command.asInstanceOf[Command] :: soFar
              case None => soFar
            }
          }
          else soFar
      })
    }
  }

  // Locations of resets are not decided
  case class Group(override val segments: List[Segment]) extends AbstractGroup(segments)

  // Locations of resets are decided
  case class GroupWithResets(override val segments: List[Segment],
                             resets: Set[Int]) extends AbstractGroup(segments) {
    assert(segments.size == resets.size + 1)
  }

  def printGroups(groups: Iterable[Group], trace: Trace): String = {
    groups.map(g => g.print(trace)).mkString("\n")
  }

  def printSegments(segments: Iterable[Segment]): String = {
    segments.map(segment => segment.printAsSet).toList.sorted.mkString(", ")
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

  class SegmentToTrainingData(trace: Trace, sumWeight: Int, commandWeight: Int) {
    def difference(left: Segment, right: Segment): Int = {
      sumDifference(left, right) + commandDifference(left, right)
    }

    def toTrainingData(segment: Segment): Int = {
      segment.getCostSum(trace) * sumWeight
      // TODO: Output a weighted vector, where the first element is the sum and has the max weight
      //  and the remaining elements represent commands and have much smaller weights
    }

    private def sumDifference(left: Segment, right: Segment): Int = {
      Math.abs(toTrainingData(left) - toTrainingData(right))
    }

    private def commandDifference(left: Segment, right: Segment): Int = {
      val leftCommands = left.indices.map(index => trace.costTraceAssociation.ghostCommandAtIndex(index)).toSet
      val rightCommands = right.indices.map(index => trace.costTraceAssociation.ghostCommandAtIndex(index)).toSet
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