package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter._
import brbo.backend2.learning.DecisionTree.{ResetLeaf, TreeClassifier, UseLeaf}
import brbo.backend2.learning.ScriptRunner.DecisionTreeLearning
import brbo.backend2.learning.SegmentClustering.{Group, Segment, printGroups}
import brbo.common.GhostVariableUtils.{counterInitialValue, resourceInitialValue, starInitialValue}
import brbo.common.ast._
import brbo.common.{MyLogger, Print}
import play.api.libs.json.Json
import tech.tablesaw.api.{IntColumn, StringColumn, Table}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Classifier {
  private val logger = MyLogger.createLogger(Classifier.getClass, debugMode = false)

  case class GroupID(value: Int) extends Print {
    def print(): String = toString
  }

  object NoneGroup extends GroupID(-34673) {
    override def print(): String = "NoneGroup"
  }

  object AllGroups extends GroupID(-4127) {
    override def print(): String = "AllGroups"
  }

  object TestGroup extends GroupID(-182827172) {
    override def print(): String = "TestGroup"
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
    } else if (label == TestGroup.print()) {
      TestGroup
    } else {
      val prefix = "GroupID("
      val suffix = ")"
      val id = label.stripPrefix(prefix).stripSuffix(suffix).toInt
      GroupID(id)
    }
  }

  def resetLabelFromString(label: String): Boolean = label.toBoolean

  abstract class BrboTable(features: List[BrboExpr]) extends Print {
    protected val tableName: String

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
            case BottomValue => row.setMissing(index)
            case _ => throw new Exception
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

  /**
   *
   * @param command The command at this trace location
   * @param index   The index of the command in the trace where this location is created
   */
  case class TraceLocation(command: Command, index: Int) extends Print {
    def print(): String = s"TraceLocation: ${command.printToIR()} (index=$index)"
  }

  case class ProgramLocation(command: Command) extends Print {
    def print(): String = s"ProgramLocation: ${command.printToIR()}"
  }

  type BrboTables = Map[GroupID, BrboTable]

  // A mapping from a location in a trace to a table for the previous store -- the store right before the location
  class TraceTables(tables: Map[TraceLocation, BrboTables], features: List[BrboExpr]) {
    def print(): String = {
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

    def toProgramTables: ProgramTables = {
      val map = tables.groupBy({ case (location, _) => location.command }).map({
        case (command, map: Map[TraceLocation, BrboTables]) =>
          val location = ProgramLocation(command)
          var summaryTables: Map[GroupID, BrboTable] = Map()

          def update(groupID: GroupID, table: BrboTable): Unit = {
            summaryTables.get(groupID) match {
              case Some(summaryTable) => summaryTable.append(table)
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

  // A mapping from a program location (i.e., a command) to a table for any of its previous store
  class ProgramTables(tables: Map[ProgramLocation, BrboTables], features: List[BrboExpr]) {
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
      if (debugMode) logger.error(map.print())
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

    def toTransformation: Map[BrboAst, BrboAst] = {
      var transforms: Map[BrboAst, BrboAst] = Map()
      results.foreach({
        case (location, results) =>
          location.command match {
            case use: Use =>
              results.get(AllGroups) match {
                case Some(UseResult(_, classifier)) =>
                  val ast = classifier.toAst(features, UseLeaf(use.update))
                  transforms = transforms + (use -> ast)
                case None =>
                case _ => throw new Exception
              }
            case resetPlaceHolder: ResetPlaceHolder =>
              results.foreach({
                case (groupID, ResetResult(_, classifier)) =>
                  val ast = classifier.toAst(features, ResetLeaf(groupID))
                  transforms = transforms + (resetPlaceHolder -> ast)
                case _ => throw new Exception
              })
            case _ => throw new Exception
          }
      })
      transforms
    }
  }

  class TableGenerationError(message: String) extends Exception

  def evaluateFunctionFromInterpreter(interpreter: Interpreter): (BrboExpr, Store) => BrboValue = {
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
                     failIfCannotFindResetPlaceHolder: Boolean): TraceTables = {
    // From group IDs to the trace node indices into which resets need to be placed
    val resetPlaceHolderMap: Map[GroupID, Set[Int]] = groups.map({
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
          // logger.trace(s"begin $begin end $end")
          val allResetPlaceHolders = trace.nodes.zipWithIndex.flatMap({
            case (node, index) =>
              val isResetPlaceHolder = node.lastTransition match {
                case Some(Transition(command, _)) => command.isInstanceOf[ResetPlaceHolder]
                case None => false
              }
              if (isResetPlaceHolder && begin <= index && index <= end)
                Some((node.lastTransition.get.command.asInstanceOf[ResetPlaceHolder], index))
              else
                None
          })
          chooseResetPlaceHolder(allResetPlaceHolders) match {
            case Some(index) => resetPlaceHolderIndices = resetPlaceHolderIndices + index
            case None =>
              val errorMessage = s"Failed to find a reset place holder between Group ${groupID.value}'s $i and ${i + 1} segment"
              // logger.trace(errorMessage)
              if (failIfCannotFindResetPlaceHolder)
                throw new TableGenerationError(errorMessage)
          }
          i = i + 1
        }
        (groupID, resetPlaceHolderIndices)
    })

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
                    resetPlaceHolderMap.get(groupID) match {
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
                })
              case _ =>
            }
          case None =>
        }
    })
    new TraceTables(traceTableMap, features)
  }

  private def chooseResetPlaceHolder(holders: Iterable[(ResetPlaceHolder, Int)]): Option[Int] = {
    if (holders.isEmpty)
      None
    else {
      // TODO: Choose more carefully
      Some(holders.head._2)
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
        if (debugMode) logger.debug(s"Classification output: $output")
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
        case (groupID, segments) => s"$groupID -> ${segments.toString()}"
      }).mkString("\n")
      s"GhostState\nresources: $resources\ncounters: $counters\nstars: $stars\nsegments:\n$segmentsString"
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

    def use(groupID: GroupID, n: Int, traceNodeIndex: Int): Unit = {
      val current = resources(groupID)
      resources = resources + (groupID -> (current + n))
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

  class ClassifierApplication(ghostStore: GhostStore, trace: Trace, debugMode: Boolean) {
    private val logger = MyLogger.createLogger(classOf[ClassifierApplication], debugMode)

    def areActualSegmentCostsSimilar(segmentClustering: SegmentClustering): Boolean = {
      // logger.traceOrError(s"${ghostStore.print()}")
      val expectedDecomposition: List[List[Segment]] = ghostStore.getSegments.values.toList.map({
        list => list.sortWith({ case (s1, s2) => s1.lessThanOrEqualTo(s2) })
      })
      logger.traceOrError(s"Expected $expectedDecomposition")
      val segments: List[Segment] = expectedDecomposition.flatten
      val actualDecomposition: List[List[Segment]] = segmentClustering.clusterSimilarSegments(trace, segments).map({
        list => list.sortWith({ case (s1, s2) => s1.lessThanOrEqualTo(s2) })
      })
      logger.traceOrError(s"Actual $actualDecomposition")
      val expected = expectedDecomposition.map(list => Group(list))
      val actual = actualDecomposition.map(list => Group(list))
      // logger.traceOrError(s"Actual groups ${printGroups(actual, trace)}")
      // logger.traceOrError(s"Expected groups ${printGroups(expected, trace)}")
      actual.forall({
        actualGroup =>
          val result = expected.exists(expectedGroup => expectedGroup.includes(actualGroup))
          if (!result)
            logger.traceOrError(s"Actual group ${actualGroup.print(trace)} is not included in the expected groups" +
              s"\n${printGroups(expected, trace)}")
          result
      })
    }
  }

  class BoundCheckClassifierApplication(val exceedBound: Boolean,
                                        ghostStore: GhostStore,
                                        trace: Trace,
                                        debugMode: Boolean) extends ClassifierApplication(ghostStore, trace, debugMode)

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

    if (debugMode) logger.error(s"# of nodes: ${trace.nodes.size}")
    trace.nodes.zipWithIndex.foreach({
      case (TraceNode(store, _), index) =>
        if (debugMode) logger.error(s"Index $index")
        if (index == trace.nodes.size - 1) {
          if (checkBound) {
            // Managed to reach the final store without violating the bound
            return new BoundCheckClassifierApplication(exceedBound = false, ghostStore, trace, debugMode)
          } else {
            return new ClassifierApplication(ghostStore, trace, debugMode)
          }
        }
        val nextCommandIndex = index + 1
        val nextCommand = trace.nodes(nextCommandIndex).lastTransition.get.command.asInstanceOf[Command]
        val location = ProgramLocation(nextCommand)
        classifierResultsMap.results.get(location) match {
          case Some(classifierResults) =>
            if (debugMode) logger.error(s"${location.print()} classifier result:\n${printClassifierResults(classifierResults)}")
            nextCommand match {
              case use: Use =>
                val label = classifierResults(AllGroups).classifier.classify(store, evaluate, classifierResultsMap.features)
                val groupID = useLabelFromString(label.name)
                ghostStore.initialize(groupID)
                if (debugMode) logger.error(s"${use.printToIR()} is decomposed into $groupID")
                if (debugMode) logger.error(s"Before:\n${ghostStore.print()}")
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
                if (debugMode) logger.error(s"After:\n${ghostStore.print()}")
              case resetPlaceHolder: ResetPlaceHolder =>
                classifierResults.foreach({
                  case (groupID, classifierResult) =>
                    ghostStore.initialize(groupID)
                    if (debugMode) logger.error(s"${resetPlaceHolder.printToIR()} is decomposed into $groupID")
                    if (debugMode) logger.error(s"Before:\n${ghostStore.print()}")
                    val toReset = {
                      val label = classifierResult.classifier.classify(store, evaluate, classifierResultsMap.features)
                      resetLabelFromString(label.name)
                    }
                    if (toReset) ghostStore.reset(groupID)
                    if (debugMode) logger.error(s"After:\n${ghostStore.print()}")
                })
              case _ => throw new Exception
            }
            if (checkBound && ghostStore.exceedBound(bound)) {
              // Early return
              logger.info(s"Bound $bound is violated under state:\n${ghostStore.print()} for the following trace decomposition:\n${trace.toTable.printAll()}")
              return new BoundCheckClassifierApplication(exceedBound = true, ghostStore, trace, debugMode)
            }
          case None =>
            if (debugMode) logger.error(s"${location.print()} does not have a classifier result")
        }
      case _ =>
    })
    throw new Exception("unreachable")
  }
}
