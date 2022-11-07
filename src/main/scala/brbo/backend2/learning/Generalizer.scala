package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter._
import brbo.backend2.learning.SegmentClustering.Group
import brbo.common.ast._
import brbo.common.{MyLogger, Print}
import tech.tablesaw.api.{IntColumn, StringColumn, Table}

object Generalizer {
  private val logger = MyLogger.createLogger(Generalizer.getClass, debugMode = false)
  // val model: DecisionTreeModel = ???

  case class GroupID(value: Int) extends Print {
    def print(): String = toString
  }

  object NoneGroup extends GroupID(-34673) {
    override def print(): String = "NoneGroup"
  }

  abstract class BrboTable[Label] extends Print {
    protected val tableName: String

    protected val table: Table = Table.create(tableName)

    protected val featureNames: List[String]

    featureNames.foreach(featureName => table.addColumns(IntColumn.create(featureName)))
    table.addColumns(StringColumn.create("Label"))

    protected def labelToString(label: Label): String

    def addRow(data: List[BrboValue], label: Label): Unit = {
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
      row.setString(data.size, labelToString(label))
    }

    def print(): String = table.printAll()

    def toJsonString: String = ???
  }

  class UseTable(override protected val featureNames: List[String]) extends BrboTable[GroupID] {
    val tableName = "UseTable"

    override def labelToString(label: GroupID): String = label.print()
  }

  class ResetTable(override protected val featureNames: List[String]) extends BrboTable[Boolean] {
    val tableName = "ResetTable"

    override def labelToString(label: Boolean): String = label.toString
  }

  /**
   *
   * @param command The command at this trace location
   * @param index   The index of the command in the trace where this location is created
   */
  case class TraceLocation(command: Command, index: Int) extends Print {
    def print(): String = s"Location: ${command.printToIR()} (index=$index)"
  }

  type ResetTables = Map[GroupID, ResetTable]

  class Tables(tables: Map[TraceLocation, Either[UseTable, ResetTables]], features: List[BrboExpr]) extends Print {
    def print(): String = {
      val separator1 = "=" * 40
      val separator2 = "*" * 60
      val featuresString = features.map(e => e.printToIR()).mkString(", ")
      val tablesString = tables.map({
        case (location, tables) =>
          val tablesString = tables match {
            case Left(useTable) => s"Use table:\n${useTable.print()}"
            case Right(resetTables) =>
              resetTables.map({
                case (id, resetTable) => s"Reset table: ${id.print()} ->\n${resetTable.print()}"
              }).toList.sorted.mkString(s"\n$separator1\n")
          }
          s"${location.print()} ->\n$tablesString"
      }).toList.sorted.mkString(s"\n$separator2\n\n")
      s"Tables:\nFeatures: $featuresString\n$tablesString"
    }
  }

  abstract class Result

  class UseResult(decisionTree: Any) extends Result {
    def generateGhostCommands(table: UseTable, features: List[BrboExpr]): Map[BrboExpr, GhostCommand] = ???
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
                     failIfCannotFindResetPlaceHolder: Boolean): Tables = {
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

    val featureNames = features.map(e => e.printToIR())
    val groupIDs = groups.keys.toSet
    var tableMap = Map[TraceLocation, Either[UseTable, ResetTables]]()
    trace.nodes.zipWithIndex.foreach({
      case (node, index) =>
        node.lastTransition match {
          case Some(Transition(command, _)) =>
            command match {
              case use: Use =>
                // Prepare the row data
                val evaluatedFeatures: List[BrboValue] =
                  features.map(feature => evaluate(feature, node.store))
                val label: GroupID = groups.find({
                  case (_, group) => group.indices.contains(index)
                }) match {
                  case Some((groupID, _)) => groupID
                  case None => NoneGroup
                }
                // Find the table and add the row data
                val location = TraceLocation(use, index)
                val useTable: UseTable = tableMap.get(location) match {
                  case Some(tables) =>
                    tables match {
                      case Left(useTable) => useTable
                      case Right(_) => throw new Exception
                    }
                  case None =>
                    val useTable = new UseTable(featureNames)
                    tableMap = tableMap + (location -> Left(useTable))
                    useTable
                }
                useTable.addRow(evaluatedFeatures, label)
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
                val evaluatedFeatures: List[BrboValue] =
                  features.map(feature => evaluate(feature, node.store))
                // Find the table and add the row data
                val location = TraceLocation(resetPlaceHolder, index)
                groupIDs.foreach({
                  groupID =>
                    val resetTable: ResetTable = tableMap.get(location) match {
                      case Some(tables) =>
                        tables match {
                          case Left(_) => throw new Exception
                          case Right(map) =>
                            map.get(groupID) match {
                              case Some(table) => table
                              case None =>
                                val resetTable = new ResetTable(featureNames)
                                tableMap = tableMap + (location -> Right(map + (groupID -> resetTable)))
                                resetTable
                            }
                        }
                      case None =>
                        val resetTable = new ResetTable(featureNames)
                        tableMap = tableMap + (location -> Right(Map(groupID -> resetTable)))
                        resetTable
                    }
                    resetTable.addRow(evaluatedFeatures, groupsToReset.contains(groupID))
                })
              case _ =>
            }
          case None =>
        }
    })

    new Tables(tableMap, features)
  }

  def chooseResetPlaceHolder(holders: Iterable[(ResetPlaceHolder, Int)]): Option[Int] = {
    if (holders.isEmpty)
      None
    else {
      // TODO: Choose more carefully
      Some(holders.head._2)
    }
  }
}
