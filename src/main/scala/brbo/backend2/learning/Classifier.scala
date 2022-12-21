package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter._
import brbo.backend2.learning.DecisionTree.{ResetLeaf, TreeClassifier, UseLeaf}
import brbo.backend2.learning.ScriptRunner.DecisionTreeLearning
import brbo.backend2.learning.SegmentClustering._
import brbo.common.GhostVariableUtils.{counterInitialValue, resourceInitialValue, starInitialValue}
import brbo.common.ast._
import brbo.common.cfg.{CFGNode, ControlFlowGraph}
import brbo.common.{MyLogger, Print}
import com.ibm.wala.util.graph.NumberedGraph
import play.api.libs.json.Json
import tech.tablesaw.api.{IntColumn, StringColumn, Table}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Classifier {
  private val logger = MyLogger.createLogger(Classifier.getClass, debugMode = false)

  case class GroupID(value: Int) extends Print {
    def print(): String = toString
  }

  object NoneGroup extends GroupID(-99) {
    // This group is used to indicate that a use or a reset command should do nothing
    override def print(): String = "NoneGroup"
  }

  object AllGroups extends GroupID(-30) {
    // For a use command, its training data (or a table) is associated with "AllGroups"
    // (which is a "virtual group"), even if the use command is decomposed into (actual) groups
    override def print(): String = "AllGroups"
  }

  object GeneralityTestGroup extends GroupID(-22) {
    // This group is used when testing the generality of a group
    // The group under test is named as "GeneralityTestGroup"
    override def print(): String = "GeneralityTestGroup"
  }

  object PrintGroup extends GroupID(-40) {
    // This group is only used for printing purposes
    override def print(): String = "PrintGroup"
  }

  abstract class Label {
    def print(): String
  }

  case class UseLabel(groupID: GroupID) extends Label {
    override def print(): String = groupID.print()
  }

  case class ResetLabel(b: Boolean) extends Label {
    override def print(): String = b.toString
  }

  def useLabelFromString(label: String): GroupID = {
    if (label == NoneGroup.print()) {
      NoneGroup
    } else if (label == GeneralityTestGroup.print()) {
      GeneralityTestGroup
    } else {
      val prefix = "GroupID("
      val suffix = ")"
      val id = label.stripPrefix(prefix).stripSuffix(suffix).toInt
      GroupID(id)
    }
  }

  def resetLabelFromString(label: String): Boolean = label.toBoolean

  abstract class BrboTable(features: List[BrboExpr]) extends Print {
    val tableName: String

    protected val table: Table = Table.create(tableName)

    val featureNames: List[String] = features.map(f => f.printToIR())
    featureNames.foreach(featureName => table.addColumns(IntColumn.create(featureName)))
    table.addColumns(StringColumn.create("Label"))

    def addRow(data: List[BrboValue], label: Label): Unit = {
      assert(data.size == featureNames.size)
      // logger.error(s"Add row [${data.map(v => v.printToIR()).mkString(",")}] with label `${labelToString(label)}`")
      val row = table.appendRow()
      data.zipWithIndex.foreach({
        case (value, index) =>
          value match {
            case Number(n, _) => row.setInt(index, n)
            case _: BrboArray | BottomValue =>
              // No need to train a classifier based on array values
              row.setMissing(index)
            case _ => throw new Exception(s"Unexpected value ${value.printToIR()}")
          }
      })
      row.setString(data.size, label.print())
    }

    def print(): String = table.printAll()

    def toJsonString: String = {
      val features = Range(0, table.rowCount()).map({
        rowIndex =>
          val row = table.row(rowIndex)
          Range(0, featureNames.size).map({
            columnIndex =>
              row.getInt(columnIndex)
          }).toList
      }).toList
      val labels = Range(0, table.rowCount()).map({
        rowIndex =>
          val row = table.row(rowIndex)
          row.getString(featureNames.size)
      }).toList
      Json.obj(("features", features), ("labels", labels)).toString()
    }

    def append(other: BrboTable): Unit = {
      val otherTable = other.table
      Range(0, otherTable.rowCount()).foreach({
        rowIndex => table.addRow(rowIndex, otherTable)
      })
    }

    def copy(): BrboTable

    def sameFeaturesDifferentLabels(): Boolean = {
      Range(0, table.rowCount()).exists({
        rowIndex =>
          Range(0, table.rowCount()).exists({
            anotherRowIndex =>
              if (anotherRowIndex > rowIndex) {
                val row = table.row(rowIndex)
                val anotherRow = table.row(anotherRowIndex)
                val sameFeatures = Range(0, features.length).forall({
                  columnIndex => row.getInt(columnIndex) == anotherRow.getInt(columnIndex)
                })
                val differentLabels = row.getObject(features.length) != anotherRow.getObject(features.length)
                sameFeatures && differentLabels
              }
              else false
          })
      })
    }
  }

  class UseTable(features: List[BrboExpr]) extends BrboTable(features) {
    val tableName = "UseTable"

    override def copy(): UseTable = {
      val newTable = new UseTable(features)
      newTable.append(this)
      newTable
    }
  }

  class ResetTable(features: List[BrboExpr]) extends BrboTable(features) {
    val tableName = "ResetTable"

    override def copy(): ResetTable = {
      val newTable = new ResetTable(features)
      newTable.append(this)
      newTable
    }
  }

  abstract class Location extends Print

  /**
   *
   * @param command The command at this trace location
   * @param index   The index of the command in the trace where this location is created
   */
  case class TraceLocation(command: Command, index: Int) extends Location {
    def print(): String = s"TraceLocation: ${command.printToIR()} (index=$index)"
  }

  case class ProgramLocation(command: Command) extends Location {
    def print(): String = s"ProgramLocation: ${command.printToIR()}"
  }

  type BrboTables = Map[GroupID, BrboTable]

  // A mapping from a location in a trace to a table for the previous store -- the store right before the location
  class TraceTables(tables: Map[TraceLocation, BrboTables], features: List[BrboExpr]) {
    def print(): String = printTables(tables.asInstanceOf[Map[Location, BrboTables]], features)

    def toProgramTables: ProgramTables = {
      val map = tables.groupBy({ case (location, _) => location.command }).map({
        case (command, map: Map[TraceLocation, BrboTables]) =>
          val location = ProgramLocation(command)
          var summaryTables: Map[GroupID, BrboTable] = Map()

          def update(groupID: GroupID, table: BrboTable): Unit = {
            if (table.sameFeaturesDifferentLabels()) {
              throw TableGenerationError(s"Inserting a table containing rows that " +
                s"have the same features but different labels\n${table.print()}")
            }
            summaryTables.get(groupID) match {
              case Some(summaryTable) =>
                val existingTable = summaryTable.copy()
                summaryTable.append(table)
                if (summaryTable.sameFeaturesDifferentLabels()) {
                  throw TableGenerationError(s"Appending `${table.tableName}` for ${groupID.print()} " +
                    s"at `${location.print()}` but now we have rows that have the same features but different labels\n" +
                    s"Existing table:\n${existingTable.print()}\n" +
                    s"Appending table:\n${table.print()}")
                }
              case None =>
                val summaryTable = table.copy()
                summaryTables = summaryTables + (groupID -> summaryTable)
            }
          }

          map.values.foreach({
            tables =>
              tables.foreach({
                case (groupID: GroupID, table: BrboTable) =>
                  table match {
                    case table: ResetTable => update(groupID, table)
                    case table: UseTable =>
                      // Merge all use tables into a single table
                      update(AllGroups, table)
                    case _ => throw new Exception
                  }
              })
          })
          (location, summaryTables)
      })
      new ProgramTables(map, features)
    }
  }

  private def printTables(tables: Map[Location, BrboTables], features: List[BrboExpr]): String = {
    val separator1 = "=" * 40
    val separator2 = "*" * 60
    val featuresString = features.map(e => e.printToIR()).mkString(", ")
    val tablesString = tables.map({
      case (location, tables) =>
        val tablesString = tables.map({
          case (id, table) =>
            table match {
              case table: UseTable => s"Use table:\n${table.print()}"
              case table: ResetTable => s"Reset table: ${id.print()} ->\n${table.print()}"
              case _ => throw new Exception
            }
        }).toList.sorted.mkString(s"\n$separator1\n")
        s"${location.print()} ->\n$tablesString"
    }).toList.sorted.mkString(s"\n$separator2\n\n")
    s"Tables:\nFeatures: $featuresString\n$tablesString"
  }

  // A mapping from a program location (i.e., a command) to a table for any of its previous store
  class ProgramTables(tables: Map[ProgramLocation, BrboTables], features: List[BrboExpr]) {
    def print(): String = printTables(tables.asInstanceOf[Map[Location, BrboTables]], features)

    def generateClassifiers(debugMode: Boolean): ClassifierResultsMap = {
      val futures = Future.traverse(tables)({
        case (location, tables) =>
          Future {
            val results = tables.map({
              case (groupID, table) => (groupID, classify(table, debugMode))
            })
            (location, results)
          }
      })
      val results = Await.result(futures, Duration.Inf).toMap
      val map = ClassifierResultsMap(results, features)
      // if (debugMode) logger.error(s"Generated classifiers: ${map.print()}")
      map
    }
  }

  abstract class ClassifierResult(val classifier: TreeClassifier) {
    def print(): String
  }

  case class UseResult(table: UseTable, override val classifier: TreeClassifier) extends ClassifierResult(classifier) {
    def print(): String = s"UseResult: ${classifier.print(table.featureNames)}"
  }

  case class ResetResult(table: ResetTable, override val classifier: TreeClassifier) extends ClassifierResult(classifier) {
    def print(): String = s"ResetResult: ${classifier.print(table.featureNames)}"
  }

  type ClassifierResults = Map[GroupID, ClassifierResult]

  def printClassifierResults(results: ClassifierResults): String = {
    results.map({
      case (groupID, result) =>
        s"${groupID.print()} -> ${result.print()}"
    }).mkString("\n")
  }

  case class ClassifierResultsMap(results: Map[ProgramLocation, ClassifierResults], features: List[BrboExpr]) {
    def print(): String = {
      val featureString = features.map(f => f.printToIR())
      val resultsString = results.map({
        case (location, results) =>
          val resultsString = results.map({
            case (groupID, result) =>
              s"${groupID.print()} -> ${result.print()}"
          }).mkString("\n")
          s"Classifier at <${location.print()}>:\n$resultsString"
      }).mkString("\n\n")
      s"ClassifierResultsMap (Features: $featureString)\n$resultsString"
    }

    def toTransformation: Map[Command, BrboAst] = {
      var transforms: Map[Command, List[BrboAst]] = Map()
      results.foreach({
        case (location, results) =>
          location.command match {
            case use: Use =>
              results.get(AllGroups) match {
                case Some(UseResult(_, classifier)) =>
                  val ast = classifier.toAst(features, UseLeaf(use.update))
                  assert(!transforms.contains(use))
                  transforms = transforms + (use -> List(ast))
                case None =>
                case _ => throw new Exception
              }
            case resetPlaceHolder: ResetPlaceHolder =>
              // To ensure determinism, we sort the list of resets based on group IDs
              val sortedResults = results.toList.sortWith({ case ((id1, _), (id2, _)) => id1.print() < id2.print() })
              sortedResults.foreach({
                case (groupID, ResetResult(_, classifier)) =>
                  val ast = classifier.toAst(features, ResetLeaf(groupID))
                  val newList = transforms.get(resetPlaceHolder) match {
                    case Some(list) =>
                      if (ast.isInstanceOf[Skip]) list
                      else ast :: list
                    case None => List(ast)
                  }
                  transforms = transforms + (resetPlaceHolder -> newList)
                case _ => throw new Exception
              })
            case _ => throw new Exception
          }
      })
      transforms.map({
        case (command, asts) =>
          if (asts.size == 1) (command, asts.head)
          else if (asts.size > 1) (command, Block(asts))
          else throw new Exception
      })
    }
  }

  def printTransformation(transformation: Map[Command, BrboAst]): String = {
    "See below for a mapping from existing ASTs to new ASTs\n" +
      transformation.map({
        case (oldCommand, newAst) =>
          s"${oldCommand.printToIR()} -> ${newAst.printToC(0)}"
      }).mkString("\n")
  }

  case class TableGenerationError(message: String) extends Exception

  def evaluateFromInterpreter(interpreter: Interpreter): (BrboExpr, Store) => BrboValue = {
    (brboExpr: BrboExpr, store: Store) =>
      try {
        interpreter.evaluateAst(InitialState(brboExpr, store, EmptyTrace)) match {
          case Interpreter.GoodState(_, _, value) => value.get
          case _ => throw new Exception
        }
      }
      catch {
        case _: Exception => BottomValue
      }
  }

  def generateTables(trace: Trace,
                     evaluate: (BrboExpr, Store) => BrboValue,
                     groups: Map[GroupID, Group],
                     features: List[BrboExpr],
                     throwIfNoResetPlaceHolder: Boolean,
                     controlFlowGraph: ControlFlowGraph): TraceTables = {
    val resetPlaceHolderCandidates: Map[GroupID, Set[Command]] =
      resetPlaceHoldersAsPostDominators(trace = trace, groups = groups, controlFlowGraph = controlFlowGraph)

    // From group IDs to the trace node indices into which resets need to be placed
    val resetPlaceHolderIndices: Map[GroupID, Set[Int]] =
      resetPlaceHolderLocations(trace, groups, resetPlaceHolderCandidates, throwIfNoResetPlaceHolder)
    // Do not specify whether to reset after the last use of the last segment (as opposed to must not reset after the last use)
    // Otherwise, the labels may introduce contradiction -- same features but different labels
    val lastUseIndices = groups.map({ case (groupID, group) => (groupID, group.last) })

    val groupIDs = groups.keys.toSet
    var traceTableMap = Map[TraceLocation, BrboTables]()
    trace.nodes.zipWithIndex.foreach({
      case (node, index) =>
        node.lastTransition match {
          case Some(Transition(command, _)) =>
            command match {
              case use: Use =>
                // Prepare the row data
                val lastStore = trace.nodes(index - 1).store
                val evaluatedFeatures: List[BrboValue] =
                  features.map(feature => evaluate(feature, lastStore))
                val groupID: GroupID = groups.find({
                  case (_, group) => group.indices.contains(index)
                }) match {
                  case Some((groupID, _)) => groupID
                  case None => NoneGroup
                }
                // Find the table and add the row data
                val traceLocation = TraceLocation(use, index)
                val useTable: UseTable = traceTableMap.get(traceLocation) match {
                  case Some(tables) =>
                    tables.get(groupID) match {
                      case Some(table) => table.asInstanceOf[UseTable]
                      case None =>
                        val useTable = new UseTable(features)
                        traceTableMap = traceTableMap + (traceLocation -> (tables + (groupID -> useTable)))
                        useTable
                    }
                  case None =>
                    val useTable = new UseTable(features)
                    traceTableMap = traceTableMap + (traceLocation -> Map(groupID -> useTable))
                    useTable
                }
                useTable.addRow(evaluatedFeatures, UseLabel(groupID))
              case resetPlaceHolder: ResetPlaceHolder =>
                // Prepare row data: Labels
                val groupsToReset: Set[GroupID] = groupIDs.filter({
                  groupID =>
                    resetPlaceHolderIndices.get(groupID) match {
                      case Some(indices) => indices.contains(index)
                      case None => false
                    }
                })
                // Prepare row data: Features
                val lastStore = trace.nodes(index - 1).store
                val evaluatedFeatures: List[BrboValue] =
                  features.map(feature => evaluate(feature, lastStore))
                // Find the table and add the row data
                val traceLocation = TraceLocation(resetPlaceHolder, index)
                groupIDs.foreach({
                  groupID =>
                    val addRow = lastUseIndices(groupID) match {
                      case Some(lastUseIndex) => index < lastUseIndex
                      case None => false
                    }
                    if (addRow) {
                      val resetTable: ResetTable = traceTableMap.get(traceLocation) match {
                        case Some(tables) =>
                          tables.get(groupID) match {
                            case Some(table) => table.asInstanceOf[ResetTable]
                            case None =>
                              val resetTable = new ResetTable(features)
                              traceTableMap = traceTableMap + (traceLocation -> (tables + (groupID -> resetTable)))
                              resetTable
                          }
                        case None =>
                          val resetTable = new ResetTable(features)
                          traceTableMap = traceTableMap + (traceLocation -> Map(groupID -> resetTable))
                          resetTable
                      }
                      resetTable.addRow(evaluatedFeatures, ResetLabel(groupsToReset.contains(groupID)))
                    }
                })
              case _ =>
            }
          case None =>
        }
    })
    new TraceTables(traceTableMap, features)
  }

  private def resetPlaceHoldersAsDominators(trace: Trace,
                                            groups: Map[GroupID, Group],
                                            controlFlowGraph: ControlFlowGraph): Map[GroupID, Set[Command]] = {
    resetPlaceHoldersAsDominatorsHelper(
      trace = trace,
      groups = groups,
      graph = controlFlowGraph.walaGraph,
      entryNode = controlFlowGraph.entryNode,
      controlFlowGraph.nodesFromCommands
    )
  }

  // We often need resets to split costs from different loop iterations, for which
  // post-dominators are good candidates
  private def resetPlaceHoldersAsPostDominators(trace: Trace,
                                                groups: Map[GroupID, Group],
                                                controlFlowGraph: ControlFlowGraph): Map[GroupID, Set[Command]] = {
    val (reversedCopiedGraph, reversedRoot) = ControlFlowGraph.reverseGraph(controlFlowGraph)
    // ControlFlowGraph.printDotToPDF("abc", ControlFlowGraph.exportToDOT())
    resetPlaceHoldersAsDominatorsHelper(
      trace = trace,
      groups = groups,
      graph = reversedCopiedGraph,
      entryNode = reversedRoot,
      controlFlowGraph.nodesFromCommands
    )
  }

  private def resetPlaceHoldersAsDominatorsHelper(trace: Trace,
                                                  groups: Map[GroupID, Group],
                                                  graph: NumberedGraph[CFGNode],
                                                  entryNode: CFGNode,
                                                  nodesFromCommands: Set[Command] => Set[CFGNode]): Map[GroupID, Set[Command]] = {
    groups.map({
      case (groupID, group) =>
        val commands = group.getCommands(trace)
        val nodes = nodesFromCommands(commands.toSet)
        // Find a node that dominates all commands from the group
        val dominator = ControlFlowGraph.closestDominator(
          graph = graph,
          entryNode = entryNode,
          nodes = nodes,
          predicate = { node: CFGNode => node.command.isInstanceOf[ResetPlaceHolder] }
        )
        dominator match {
          case Some(dominator) => (groupID, Set(dominator.command))
          case None => (groupID, Set[Command]())
        }
    })
  }

  private def resetPlaceHolderLocations(trace: Trace,
                                        groups: Map[GroupID, Group],
                                        resetPlaceHolderCandidates: Map[GroupID, Set[Command]],
                                        throwIfNoResetPlaceHolder: Boolean): Map[GroupID, Set[Int]] = {
    val nodesWithIndex = trace.nodes.zipWithIndex
    groups.map({
      case (groupID, group) =>
        var resetPlaceHolderIndices: Set[Int] = Set()
        var i = 0
        while (i + 1 < group.segments.size) {
          val begin = {
            val current = group.segments(i)
            current.indices.last + 1
          }
          val end = {
            val next = group.segments(i + 1)
            next.indices.head - 1
          }
          val allPossibleResetPlaceHolders: Iterable[(ResetPlaceHolder, Int)] = nodesWithIndex.flatMap({
            case (node, index) =>
              val isResetPlaceHolder = node.lastTransition match {
                case Some(Transition(command, _)) => command.isInstanceOf[ResetPlaceHolder]
                case _ => false
              }
              if (isResetPlaceHolder && begin <= index && index <= end)
                Some((node.lastTransition.get.command.asInstanceOf[ResetPlaceHolder], index))
              else
                None
          })
          resetPlaceHolderFromCandidates(allPossibleResetPlaceHolders, resetPlaceHolderCandidates(groupID)) match {
            case Some(index) => resetPlaceHolderIndices = resetPlaceHolderIndices + index
            case None =>
              val errorMessage = s"Failed to find a reset place holder between " +
                s"${groupID.print()}'s $i and ${i + 1} segment (index range: [$begin, $end])\n" +
                s"${printDecomposition(trace, groups)}"
              if (throwIfNoResetPlaceHolder) throw TableGenerationError(errorMessage)
          }
          i = i + 1
        }
        (groupID, resetPlaceHolderIndices)
    })
  }

  private def resetPlaceHolderFromCandidates(allPossibleResetPlaceHolders: Iterable[(ResetPlaceHolder, Int)],
                                             candidates: Set[Command]): Option[Int] = {
    if (allPossibleResetPlaceHolders.isEmpty || candidates.isEmpty)
      None
    else {
      // logger.trace(s"dominator: $dominator")
      // holders.foreach({ case (holder, i) => logger.trace(s"index $i: ${holder}")})
      // Let the reset place holder be the dominator
      allPossibleResetPlaceHolders.find({ case (resetPlaceHolder, _) => candidates.contains(resetPlaceHolder) }) match {
        case Some((_, index)) => Some(index)
        case None => None
      }
    }
  }

  def classify(table: BrboTable, debugMode: Boolean): ClassifierResult = {
    val classifier = classifyInternal(table, debugMode)
    table match {
      case table: ResetTable => ResetResult(table, classifier)
      case table: UseTable => UseResult(table, classifier)
      case _ => throw new Exception
    }
  }

  private def classifyInternal(table: BrboTable, debugMode: Boolean): TreeClassifier = {
    ScriptRunner.run(table.toJsonString, DecisionTreeLearning(printTree = false), debugMode) match {
      case Some(output) =>
        // if (debugMode) logger.debug(s"Classification output: $output")
        DecisionTree.parse(output)
      case None => throw new Exception
    }
  }

  class GhostStore {
    private var resources: Map[GroupID, Int] = Map()
    private var counters: Map[GroupID, Int] = Map()
    private var stars: Map[GroupID, Int] = Map()
    private var segments: Map[GroupID, List[Segment]] = Map()

    def getSegments: Map[GroupID, List[Segment]] = segments

    def initialize(groupID: GroupID): Unit = {
      if (!counters.contains(groupID))
        counters = counters + (groupID -> counterInitialValue)
      if (!stars.contains(groupID))
        stars = stars + (groupID -> starInitialValue)
      if (!resources.contains(groupID))
        resources = resources + (groupID -> resourceInitialValue)
    }

    def print(): String = {
      val segmentsString = segments.map({
        case (groupID, segments) => s"${groupID.print()} -> ${printSegments(segments)}"
      }).mkString("\n")
      s"GhostState\nresources: ${printMap(resources)}\ncounters: ${printMap(counters)}\nstars: ${printMap(stars)}\nsegments:\n$segmentsString"
    }

    private def printMap(map: Map[GroupID, Int]): String = {
      map.map({ case (id, value) => s"${id.print()} -> $value" }).mkString(", ")
    }

    def reset(groupID: GroupID): Unit = {
      updateStar(groupID)
      resetResource(groupID)
      incrementCounter(groupID)
      val segmentList: List[Segment] = segments.get(groupID) match {
        case Some(list) => list
        case None => Nil
      }
      segments = segments + (groupID -> (segmentList :+ Segment(indices = Nil)))
    }

    private def incrementCounter(groupID: GroupID): Unit = {
      val current = counters(groupID)
      counters = counters + (groupID -> (current + 1))
    }

    private def updateStar(groupID: GroupID): Unit = {
      val current = stars(groupID)
      val resource = resources(groupID)
      stars = stars + (groupID -> Math.max(resource, current))
    }

    private def resetResource(groupID: GroupID): Unit = {
      resources = resources + (groupID -> 0)
    }

    def use(groupID: GroupID, cost: Int, traceNodeIndex: Int): Unit = {
      val current = resources(groupID)
      resources = resources + (groupID -> (current + cost))
      val segmentList: List[Segment] = segments.get(groupID) match {
        case Some(segmentList) =>
          val segment = segmentList.last.add(traceNodeIndex)
          segmentList.slice(0, segmentList.length - 1) :+ segment
        case None => List(Segment(List(traceNodeIndex)))
      }
      segments = segments + (groupID -> segmentList)
    }

    def exceedBound(bound: Option[Int]): Boolean = {
      bound match {
        case Some(bound) =>
          val approximation = resources.map({
            case (groupID, resource) =>
              val counter = counters(groupID)
              val star = stars(groupID)
              resource + star * counter
          }).sum
          approximation > bound
        case None => false
      }
    }
  }

  object GhostStore {
    def removeLastEmptySegment(segments: Map[GroupID, List[Segment]]): Map[GroupID, List[Segment]] = {
      segments.map({
        case (groupID, segments) =>
          val newSegments = {
            if (segments.nonEmpty) {
              if (segments.last.isEmpty) segments.slice(0, segments.size - 1)
              else segments
            }
            else segments
          }
          (groupID, newSegments)
      })
    }
  }

  case class DecomposedTraceNode(index: Int, transition: Transition, groupID: GroupID)

  class ClassifierApplication(ghostStore: GhostStore, trace: Trace, decomposedTrace: List[DecomposedTraceNode], debugMode: Boolean) {
    private val logger = MyLogger.createLogger(classOf[ClassifierApplication], debugMode)

    private def print(groupsOfSegments: List[List[Segment]]): String = {
      groupsOfSegments.map(segments => segments.map(s => s.printAsSet).mkString(", ")).mkString("\n")
    }

    def areActualSegmentCostsSimilar(segmentClustering: SegmentClustering,
                                     differentIfNoSegmentAfterDecomposition: Boolean,
                                     removeLastEmptySegmentAfterDecomposition: Boolean,
                                     stringBuilder: mutable.StringBuilder): Boolean = {
      val expectedDecomposition: List[List[Segment]] = {
        val segments = {
          if (removeLastEmptySegmentAfterDecomposition) GhostStore.removeLastEmptySegment(ghostStore.getSegments)
          else ghostStore.getSegments
        }
        segments.values.toList.map({
          list => list.sortWith({ case (s1, s2) => s1.lessThan(s2) })
        })
      }
      stringBuilder.append(s"\nSegment clusters that are considered to be similar:\n${print(expectedDecomposition)}\n")
      val segments: List[Segment] = expectedDecomposition.flatten
      // stringBuilder.append(s"Final ghost state after trace decomposition: ${ghostStore.print()}\n")
      if (segments.forall(segment => segment.isEmpty)) {
        stringBuilder.append(s"Empty segments are vacuously similar to each other. Return ${!differentIfNoSegmentAfterDecomposition}.\n")
        return !differentIfNoSegmentAfterDecomposition
      }
      val actualDecomposition: List[List[Segment]] = segmentClustering.clusterSimilarSegments(trace, segments).map({
        list => list.sortWith({ case (s1, s2) => s1.lessThan(s2) })
      })
      stringBuilder.append(s"Segment clusters that are actually similar:\n${print(actualDecomposition)}\n\n")
      val expected = expectedDecomposition.map(segments => Group(segments))
      val actual = actualDecomposition.map(segments => Group(segments))
      if (expected.nonEmpty && actual.isEmpty)
        return false // calling forall on an empty list returns true
      // logger.traceOrError(s"Actual groups ${printGroups(actual, trace)}")
      // logger.traceOrError(s"Expected groups ${printGroups(expected, trace)}")
      if (expected.size != actual.size)
        return false
      expected.zip(actual).forall({ case (expectedGroup, actualGroup) => expectedGroup.sameAs(actualGroup) })
    }

    lazy val printDecomposedTrace: String = {
      val table: Table = Table.create("")
      val commands: ArrayBuffer[String] = ArrayBuffer()
      val costs: ArrayBuffer[String] = ArrayBuffer()
      val indices: ArrayBuffer[String] = ArrayBuffer()
      val groupIDs: Map[GroupID, ArrayBuffer[String]] = decomposedTrace.map(node => (node.groupID, ArrayBuffer[String]())).toMap
      var values: Map[String, ArrayBuffer[String]] = Map()
      decomposedTrace.foreach({
        case DecomposedTraceNode(index, transition, groupID) =>
          indices.append(index.toString)
          val (commandString, costString) = transition.print(onlyGhostCommand = true, commandMaxLength = 30)
          commands.append(commandString)
          costs.append(costString)
          groupIDs.foreach({
            case (groupID2, array) =>
              if (groupID2 == groupID) array.append("*")
              else array.append("")
          })
          trace.variables.foreach({
            case (name, _) =>
              val value = trace.nodes(index).store.printValue(name)
              values.get(name) match {
                case Some(array) => array.append(value)
                case None =>
                  val array = ArrayBuffer[String]()
                  array.append(value)
                  values = values + (name -> array)
              }
          })
      })
      table.addColumns(StringColumn.create("Index", indices: _*))
      table.addColumns(StringColumn.create("Command", commands: _*))
      table.addColumns(StringColumn.create("Cost", costs: _*))
      groupIDs.foreach({
        case (groupID, array) =>
          table.addColumns(StringColumn.create(groupID.print(), array: _*))
      })
      values.toList.sortWith({ case (p1, p2) => p1._1 < p2._1 }).foreach({
        case (identifier, values) =>
          table.addColumns(StringColumn.create(identifier, values: _*))
      })
      table.printAll()
    }
  }

  class BoundCheckClassifierApplication(val exceedBound: Boolean,
                                        ghostStore: GhostStore,
                                        trace: Trace,
                                        decomposedTrace: List[DecomposedTraceNode],
                                        debugMode: Boolean) extends ClassifierApplication(ghostStore, trace, decomposedTrace, debugMode)

  // Interpret the trace with the given classifier, to see if at some point
  // the cost approximation goes beyond the given bound
  def applyClassifiers(boundExpression: Option[BrboExpr],
                       trace: Trace,
                       evaluate: (BrboExpr, Store) => BrboValue,
                       classifierResultsMap: ClassifierResultsMap,
                       debugMode: Boolean): ClassifierApplication = {
    val checkBound = boundExpression.isDefined
    val bound: Option[Int] = boundExpression match {
      case Some(boundExpression) =>
        evaluate(boundExpression, trace.nodes.head.store) match {
          case Number(n, _) => Some(n)
          case _ => throw new Exception
        }
      case None => None
    }
    val ghostStore = new GhostStore
    var decomposedTrace: List[DecomposedTraceNode] = Nil

    // if (debugMode) logger.error(s"# of nodes: ${trace.nodes.size}")
    trace.nodes.zipWithIndex.foreach({
      case (TraceNode(store, _), index) =>
        if (index == trace.nodes.size - 1) {
          if (checkBound) {
            // Managed to reach the final store without violating the bound
            return new BoundCheckClassifierApplication(exceedBound = false, ghostStore, trace, decomposedTrace = decomposedTrace.reverse, debugMode)
          } else {
            return new ClassifierApplication(ghostStore, trace, decomposedTrace = decomposedTrace.reverse, debugMode)
          }
        }
        val nextCommandIndex = index + 1
        val nextTransition = trace.nodes(nextCommandIndex).lastTransition.get
        val nextCommand = nextTransition.command.asInstanceOf[Command]
        val location = ProgramLocation(nextCommand)
        classifierResultsMap.results.get(location) match {
          case Some(classifierResults) =>
            // if (debugMode) logger.error(s"${location.print()} classifier result:\n${printClassifierResults(classifierResults)}")
            nextCommand match {
              case use: Use =>
                val label = classifierResults(AllGroups).classifier.classify(store, evaluate, classifierResultsMap.features)
                val groupID = useLabelFromString(label.name)
                ghostStore.initialize(groupID)
                // if (debugMode) logger.error(s"[Index $index] ${use.printToIR()} is decomposed into ${groupID.print()}")
                decomposedTrace = DecomposedTraceNode(nextCommandIndex, nextTransition, groupID) :: decomposedTrace
                lazy val beforeString = ghostStore.print()
                evaluate(use.condition, store) match {
                  case Bool(b, _) =>
                    if (b && groupID != NoneGroup) {
                      evaluate(use.update, store) match {
                        case Number(update, _) => ghostStore.use(groupID, update, traceNodeIndex = nextCommandIndex)
                        case _ => throw new Exception
                      }
                    }
                  case _ => throw new Exception
                }
                lazy val afterString = ghostStore.print()
              // if (debugMode && beforeString != afterString) {
              //   logger.error(s"Before:\n$beforeString")
              //   logger.error(s"After:\n$afterString")
              // }
              case _: ResetPlaceHolder =>
                classifierResults.foreach({
                  case (groupID, classifierResult) =>
                    ghostStore.initialize(groupID)
                    // if (debugMode) logger.error(s"[Index $index] ${resetPlaceHolder.printToIR()} is decomposed into ${groupID.print()}")
                    lazy val beforeString = ghostStore.print()
                    val toReset = {
                      val label = classifierResult.classifier.classify(store, evaluate, classifierResultsMap.features)
                      resetLabelFromString(label.name)
                    }
                    if (toReset) {
                      ghostStore.reset(groupID)
                      decomposedTrace = DecomposedTraceNode(nextCommandIndex, nextTransition, groupID) :: decomposedTrace
                    }
                    lazy val afterString = ghostStore.print()
                  // if (debugMode && beforeString != afterString) {
                  //   logger.error(s"Before:\n$beforeString")
                  //   logger.error(s"After:\n$afterString")
                  // }
                })
              case _ => throw new Exception
            }
            if (checkBound && ghostStore.exceedBound(bound)) {
              // Early return
              logger.info(s"Bound $bound is violated under state:\n${ghostStore.print()} for the following " +
                s"trace decomposition:\n${trace.toTable(printStores = true, onlyGhostCommand = false)._1.printAll()}")
              return new BoundCheckClassifierApplication(exceedBound = true, ghostStore, trace, decomposedTrace = decomposedTrace.reverse, debugMode)
            }
          case None =>
          // if (debugMode) logger.error(s"${location.print()} does not have a classifier result")
        }
    })
    throw new Exception("unreachable")
  }
}
