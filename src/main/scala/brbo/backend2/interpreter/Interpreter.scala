package brbo.backend2.interpreter

import brbo.backend2.interpreter.Interpreter._
import brbo.common.ast.BrboAstUtils.immediateParentStatements
import brbo.common.ast._
import brbo.common.{BrboType, MyLogger, PreDefinedFunctions, Print}
import tech.tablesaw.api.{IntColumn, StringColumn, Table}

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, Map}
import scala.collection.mutable.ArrayBuffer

class Interpreter(val brboProgram: BrboProgram, debugMode: Boolean = false) {
  protected val logger: MyLogger = MyLogger.createLogger(classOf[Interpreter], debugMode)
  private val parentStatements: Map[BrboAst, Statement] =
    (brboProgram.mainFunction :: brboProgram.functions).flatMap(f => immediateParentStatements(f.bodyWithInitialization)).toMap

  def execute(inputValues: List[BrboValue]): FlowEndState = {
    try {
      // logger.traceOrError(s"Execute with inputs: ${inputValues.map(v => v.printToIR())}")
      val state = evaluateFunction(brboProgram.mainFunction, inputValues, EmptyTrace)
      // logger.traceOrError(s"Final state: ${printState(state)}")
      state
    } catch {
      case TraceTooLongException(lastState) => lastState
    }
  }

  def evaluateFunction(brboFunction: BrboFunction, inputValues: List[BrboValue], lastTrace: Trace): FlowEndState = {
    val parameters = brboFunction.parameters
    assert(parameters.length == inputValues.length)
    val initialStore = parameters.zip(inputValues).foldLeft(new Store())({
      case (store, (parameter, inputValue)) => store.set(parameter, inputValue)
    })
    // logger.traceOrError(s"Evaluate function `${brboFunction.identifier}` with initial store $initialStore")
    val initialState = InitialState(brboFunction.bodyWithInitialization, initialStore, lastTrace.add(TraceNode(initialStore, None)))
    evaluateAst(initialState)
  }

  def evaluateAst(initialState: InitialState): FlowEndState = {
    // println(s"evaluateAst ${printState(initialState)}")
    initialState.ast match {
      case _: BrboExpr => evaluateExpr(initialState)
      case _: Command => evaluateCommand(initialState)
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            var state: FlowEndState = GoodState(initialState.store, initialState.trace, None)
            var i = 0
            while (i < asts.length) {
              state match {
                case JumpState(store, trace, jump) =>
                  return state
                case GoodState(store, trace, _) =>
                  state = evaluateAst(InitialState(asts(i), store, trace))
                case _ => throw new Exception
              }
              i = i + 1
            }
            state
          case ITE(condition, thenAst, elseAst, _) =>
            evaluateExpr(InitialState(condition, initialState.store, initialState.trace)) match {
              case GoodState(store, trace, value) =>
                value match {
                  case Some(Bool(b, _)) =>
                    if (b) evaluateAst(InitialState(thenAst, store, trace))
                    else evaluateAst(InitialState(elseAst, store, trace))
                  case _ => throw new Exception
                }
              case _ => throw new Exception
            }
          case loop@Loop(condition, loopBody, _) =>
            evaluateExpr(InitialState(condition, initialState.store, initialState.trace)) match {
              case GoodState(store, trace, value) =>
                value match {
                  case Some(Bool(b, _)) =>
                    if (b) {
                      evaluateAst(InitialState(loopBody, store, trace)) match {
                        case GoodState(store, trace, _) =>
                          evaluateAst(InitialState(loop, store, trace))
                        case JumpState(store, trace, jump) =>
                          jump match {
                            case Interpreter.BreakJump =>
                              GoodState(store, trace, value)
                            case Interpreter.ContinueJump =>
                              evaluateAst(InitialState(loop, store, trace))
                          }
                        case _ => throw new Exception
                      }
                    }
                    else GoodState(store, trace, None)
                  case _ => throw new Exception
                }
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  @tailrec
  private def evaluateCommand(initialState: InitialState): FlowEndState = {
    // println(s"evaluateAst ${printState(initialState)}")
    val ast = initialState.ast
    ast match {
      case _: CFGOnly | _: CexPathOnly => throw new Exception()
      case _: VariableDeclaration | _: Assignment =>
        evaluateAssignment(initialState, ast)
      case _: Skip | _: ResetPlaceHolder =>
        GoodState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), None)
      case _: Break =>
        JumpState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), BreakJump)
      case _: Continue =>
        JumpState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), ContinueJump)
      case Return(expression, _) =>
        expression match {
          case Some(expression) =>
            evaluateExpr(InitialState(expression, initialState.store, initialState.trace)) match {
              case goodState@GoodState(store, _, value) =>
                GoodState(store, appendToTraceFrom(goodState, Transition(ast)), value)
              case _ => throw new Exception
            }
          case None => GoodState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), None)
        }
      case _: Assume => throw new Exception
      case use@Use(_, update, condition, _) =>
        evaluateExpr(InitialState(condition, initialState.store, initialState.trace)) match {
          case GoodState(store, trace, value) =>
            value match {
              case Some(Bool(b, _)) =>
                if (!b) GoodState(store, trace, None)
                else {
                  evaluateExpr(InitialState(update, store, trace)) match {
                    case GoodState(store, trace, value) =>
                      val currentAccumulation = store.get(use.resourceVariable) match {
                        case Number(n, _) => n
                        case _ => throw new Exception
                      }
                      val (newAccumulation, cost) = value.get match {
                        case Number(n, _) => (Number(n + currentAccumulation), Some(n))
                        case _ => throw new Exception
                      }
                      val newStore = store.set(use.resourceVariable, newAccumulation)
                      val lastState = GoodState(newStore, trace, None)
                      GoodState(newStore, appendToTraceFrom(lastState, Transition(use, cost)), None)
                    case _ => throw new Exception
                  }
                }
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      case reset@Reset(_, condition, _) =>
        evaluateExpr(InitialState(condition, initialState.store, initialState.trace)) match {
          case GoodState(store, trace, value) =>
            value match {
              case Some(Bool(b, _)) =>
                val lastState = GoodState(store, trace, None)
                if (!b) lastState
                else {
                  (store.get(reset.resourceVariable), store.get(reset.starVariable)) match {
                    case (Number(lastSegmentCost, _), Number(maxSegmentCost, _)) =>
                      val largerCost = if (lastSegmentCost >= maxSegmentCost) lastSegmentCost else maxSegmentCost
                      val newCounterValue = store.get(reset.counterVariable) match {
                        case Number(n, _) => Number(n + 1)
                        case _ => throw new Exception
                      }
                      val newStore =
                        store.set(reset.starVariable, Number(largerCost))
                          .set(reset.counterVariable, newCounterValue)
                          .set(reset.resourceVariable, Number(0))
                      GoodState(newStore, appendToTraceFrom(lastState, Transition(reset)), None)
                    case _ => throw new Exception
                  }
                }
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      case LabeledCommand(_, command, _) =>
        evaluateCommand(InitialState(command, initialState.store, appendToTraceFrom(initialState, Transition(ast))))
    }
  }

  private def evaluateExpr(initialState: InitialState): FlowEndState = {
    val ast = initialState.ast
    ast match {
      case brboValue: BrboValue =>
        GoodState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), Some(brboValue))
      case identifier@Identifier(_, _, _) =>
        GoodState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), Some(initialState.store.get(identifier)))
      case Addition(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Number(n1, _), Number(n2, _)) => Number(n1 + n2)
                case _ => throw new Exception
              }
          }
        )
      case Subtraction(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Number(n1, _), Number(n2, _)) => Number(n1 - n2)
                case _ => throw new Exception
              }
          }
        )
      case Multiplication(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Number(n1, _), Number(n2, _)) => Number(n1 * n2)
                case _ => throw new Exception
              }
          }
        )
      case Negation(expression, _) =>
        val newState = evaluateExpr(InitialState(expression, initialState.store, initialState.trace))
        newState match {
          case GoodState(store, _, Some(Bool(b, _))) =>
            GoodState(store, appendToTraceFrom(newState, Transition(ast)), Some(Bool(!b)))
          case _ => throw new Exception()
        }
      case LessThan(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Number(n1, _), Number(n2, _)) => Bool(n1 < n2)
                case _ => throw new Exception
              }
          }
        )
      case Equal(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Number(n1, _), Number(n2, _)) => Bool(n1 == n2)
                case (Bool(b1, _), Bool(b2, _)) => Bool(b1 == b2)
                case _ => throw new Exception
              }
          }
        )
      case And(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Bool(b1, _), Bool(b2, _)) => Bool(b1 && b2)
                case _ => throw new Exception
              }
          }
        )
      case Or(left, right, _) =>
        evaluateBinaryExpr(ast, left, right, initialState,
          {
            (left: BrboValue, right: BrboValue) =>
              (left, right) match {
                case (Bool(b1, _), Bool(b2, _)) => Bool(b1 || b2)
                case _ => throw new Exception
              }
          }
        )
      case FunctionCallExpr(identifier, arguments, _, _) =>
        val lastTransition = Transition(ast)
        PreDefinedFunctions.functions.find(f => f.name == identifier) match {
          case Some(specialFunction) =>
            specialFunction.name match {
              case PreDefinedFunctions.VerifierError.name | PreDefinedFunctions.Abort.name =>
                throw new BadStateException(initialState.store, appendToTraceFrom(initialState, lastTransition))
              case PreDefinedFunctions.VerifierNondetInt.name | PreDefinedFunctions.NdInt.name =>
                val random = new scala.util.Random(seed = initialState.store.toString.hashCode())
                GoodState(initialState.store, appendToTraceFrom(initialState, lastTransition), Some(Number(random.nextInt())))
              case PreDefinedFunctions.NdInt2.name =>
                evaluateExpr(InitialState(arguments.head, initialState.store, initialState.trace)) match {
                  case GoodState(store, trace, Some(Number(lowerBound, _))) =>
                    evaluateExpr(InitialState(arguments(1), store, trace)) match {
                      case lastState@GoodState(store, _, Some(Number(upperBound, _))) =>
                        if (upperBound < lowerBound) {
                          // logger.error(s"Calling ${PreDefinedFunctions.NdInt2.name} with (lower: $lowerBound, upper: $upperBound)")
                          throw new BadStateException(store, appendToTraceFrom(lastState, lastTransition))
                          // GoodState(store, appendToTraceFrom(lastState, lastTransition), Some(Number(-1)))
                        } else {
                          val random = new scala.util.Random(seed = lowerBound + upperBound)
                          val value = Number(lowerBound + random.nextInt(upperBound - lowerBound + 1))
                          GoodState(store, appendToTraceFrom(lastState, lastTransition), Some(value))
                        }
                      case _ => throw new Exception
                    }
                  case _ => throw new Exception
                }
              case PreDefinedFunctions.NdBool.name =>
                val random = new scala.util.Random(seed = initialState.store.toString.hashCode())
                GoodState(initialState.store, appendToTraceFrom(initialState, lastTransition), Some(Bool(random.nextBoolean())))
              case PreDefinedFunctions.Assume.name => throw new Exception
              case PreDefinedFunctions.BoundAssertion.name => throw new Exception
              case PreDefinedFunctions.Uninitialize.name => throw new Exception
              case PreDefinedFunctions.MostPreciseBound.name | PreDefinedFunctions.LessPreciseBound.name =>
                GoodState(initialState.store, appendToTraceFrom(initialState, lastTransition), None)
              case _ =>
                evaluateFunctionCall(initialState, specialFunction.cRepresentation, arguments)
            }
          case None =>
            brboProgram.functions.find(f => f.identifier == identifier) match {
              case Some(function) => evaluateFunctionCall(initialState, function, arguments)
              case None => throw new Exception(s"Evaluating a function that is not defined: $ast")
            }
        }
      case ITEExpr(condition, thenExpr, elseExpr, _) =>
        evaluateExpr(InitialState(condition, initialState.store, initialState.trace)) match {
          case GoodState(store, trace, value) =>
            value match {
              case Some(Bool(b, _)) =>
                if (b) evaluateExpr(InitialState(thenExpr, store, trace))
                else evaluateExpr(InitialState(elseExpr, store, trace))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      case ArrayRead(array, index, _) =>
        evaluateExpr(InitialState(array, initialState.store, initialState.trace)) match {
          case GoodState(store, trace, arrayValue) =>
            arrayValue match {
              case Some(BrboArray(arrayValue, _, _)) =>
                evaluateExpr(InitialState(index, store, trace)) match {
                  case GoodState(store, trace, indexValue) =>
                    indexValue match {
                      case Some(Number(indexValue, _)) =>
                        val value = arrayValue(indexValue).asInstanceOf[BrboValue]
                        GoodState(store, trace, Some(value))
                      case _ => throw new Exception
                    }
                  case _ => throw new Exception
                }
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      case ArrayLength(array, _) =>
        evaluateExpr(InitialState(array, initialState.store, initialState.trace)) match {
          case GoodState(store, trace, arrayValue) =>
            arrayValue match {
              case Some(BrboArray(arrayValue, _, _)) =>
                GoodState(store, trace, Some(Number(arrayValue.length)))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
    }
  }

  private def evaluateBinaryExpr(thisExpr: BrboAst, left: BrboExpr, right: BrboExpr, initialState: InitialState,
                                 composeValues: (BrboValue, BrboValue) => BrboValue): FlowEndState = {
    val leftState = evaluateExpr(InitialState(left, initialState.store, initialState.trace))
    val rightState = evaluateExpr(InitialState(right, leftState.store, leftState.trace))
    (leftState, rightState) match {
      case (GoodState(_, _, Some(leftValue)), GoodState(_, _, Some(rightValue))) =>
        GoodState(
          rightState.store,
          appendToTraceFrom(rightState, Transition(thisExpr)),
          Some(composeValues(leftValue, rightValue))
        )
      case _ => throw new Exception
    }
  }

  private def evaluateFunctionCall(initialState: InitialState,
                                   function: BrboFunction, arguments: List[BrboExpr]): FlowEndState = {
    // logger.traceOrError(s"Evaluate function call: `${function.identifier}` with arguments $arguments")
    val (state, argumentValues) = arguments.foldLeft(
      GoodState(initialState.store, initialState.trace, None): FlowEndState,
      Nil: List[BrboValue]
    )({
      case ((state, argumentValues), argument) =>
        state match {
          case GoodState(store, trace, _) =>
            evaluateExpr(InitialState(argument, store, trace)) match {
              case goodState@GoodState(_, _, value) =>
                // logger.traceOrError(s"Actual argument `$argument` is evaluated as `$value`")
                (goodState, value.get :: argumentValues)
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
    })
    state match {
      case GoodState(_, trace, _) =>
        evaluateFunction(function, argumentValues.reverse, trace) match {
          case GoodState(_, trace, value) =>
            // Restore to the store before the function call
            GoodState(initialState.store, trace, value)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  private def evaluateAssignment(initialState: InitialState, assignment: BrboAst): FlowEndState = {
    val (identifier, value) = assignment match {
      case Assignment(identifier, expression, _) => (identifier, expression)
      case VariableDeclaration(identifier, initialValue, _) => (identifier, initialValue)
      case _ => throw new Exception
    }
    evaluateExpr(InitialState(value, initialState.store, initialState.trace)) match {
      case GoodState(store, trace, value) =>
        val newStore = store.set(identifier, value.get)
        val lastState = GoodState(newStore, trace, None)
        GoodState(newStore, appendToTraceFrom(lastState, Transition(assignment)), None)
      case _ => throw new Exception
    }
  }

  private def appendToTraceFrom(lastState: State, lastTransition: Transition): Trace = {
    val (trace, store) = lastState match {
      case state: FlowBeginState => (state.trace, state.store)
      case state: FlowEndState => (state.trace, state.store)
      case _ => throw new Exception
    }
    val node = TraceNode(store, Some(lastTransition))
    trace.add(node)
  }
}

object Interpreter {
  private val MAXIMUM_TRACE_LENGTH = -1 // -1 means no constraint

  class Store {
    private var map = new HashMap[String, BrboValue]

    def set(variable: Identifier, value: BrboValue): Store = {
      assert(variable.typ == variable.typ)
      val store = new Store
      store.map = map + (variable.name -> value)
      store
    }

    def get(variable: Identifier): BrboValue = get(variable.name)

    override def toString: String = {
      val values = map.map({
        case (identifier, value) => s"$identifier -> ${value.printToIR()}"
      }).toList.sorted.mkString(", ")
      s"Store: ($values)"
    }

    def getVariables: Map[String, BrboType.T] = {
      map.map({
        case (name, value) =>
          val typ = value match {
            case _: Number => BrboType.INT
            case _: Bool => BrboType.BOOL
            case _ => throw new Exception("Unknown Type")
          }
          (name, typ)
      })
    }

    def printValue(variable: String): String = {
      try {
        get(variable) match {
          case Number(n, _) => n.toString
          case Bool(b, _) => b.toString
          case _ => throw new Exception
        }
      } catch {
        case _: VariableNotFoundException => ""
      }
    }

    private def get(variable: String): BrboValue = map.getOrElse(variable,
      throw new VariableNotFoundException(s"Variable `$variable` is not defined in ${this.toString}"))
  }

  class VariableNotFoundException(message: String) extends Exception {
    override def toString: String = s"$message\n${super.toString}"
  }

  /**
   *
   * @param command The command or expression in this transition
   * @param cost    The cost (if any) from this transition
   */
  case class Transition(command: BrboAst, cost: Option[Int] = None) {
    override def toString: String = {
      val commandString = command match {
        case command: Command => command.printToIR()
        case _ => throw new Exception
      }
      cost match {
        case Some(cost) => s"$commandString <cost=$cost>"
        case None => commandString
      }
    }

    def print(onlyGhostCommand: Boolean, commandMaxLength: Int): (String, String) = {
      if (command.isInstanceOf[GhostCommand] || !onlyGhostCommand) {
        val commandString = {
          val commandString = command.printToC(0)
          if (commandString.length > commandMaxLength) s"${commandString.slice(0, commandMaxLength)}..."
          else commandString
        }
        val costString = {
          cost match {
            case Some(value) => s"$value"
            case None => ""
          }
        }
        (commandString, costString)
      }
      else ("", "")
    }
  }

  abstract class State

  /**
   *
   * @param store The store where the execution terminates -- we do not know what is the next transition
   * @param trace The trace from the initial state to the current (terminal) state
   */
  abstract class FlowEndState(val store: Store,
                              val trace: Trace) extends State

  /**
   *
   * @param store The store where the execution begins
   * @param trace The trace from the initial state to the current (non-terminal) state
   */
  abstract class FlowBeginState(val ast: BrboAst,
                                val store: Store,
                                val trace: Trace) extends State

  case class GoodState(override val store: Store,
                       override val trace: Trace,
                       value: Option[BrboValue]) extends FlowEndState(store, trace) {
    if (MAXIMUM_TRACE_LENGTH >= 0 && trace.nodes.length >= MAXIMUM_TRACE_LENGTH)
      throw TraceTooLongException(this)
  }

  sealed trait Jump

  object BreakJump extends Jump

  object ContinueJump extends Jump

  case class JumpState(override val store: Store,
                       override val trace: Trace,
                       jump: Jump) extends FlowEndState(store, trace)

  case class InitialState(override val ast: BrboAst,
                          override val store: Store,
                          override val trace: Trace) extends FlowBeginState(ast, store, trace)

  class BadStateException(val store: Store, val trace: Trace) extends Exception {
    override def toString: String = {
      val superString = super.toString
      s"$superString\n${trace.toTable(printStores = true, onlyGhostCommand = false)._1.printAll()}"
    }
  }

  case class TraceTooLongException(flowEndState: FlowEndState) extends Exception {
    override def toString: String = {
      val superString = super.toString
      s"$superString\n${flowEndState.trace.toTable(printStores = true, onlyGhostCommand = false)._1.printAll()}"
    }
  }

  /**
   *
   * @param store          The current store
   * @param lastTransition The last transition that was evaluated
   */
  case class TraceNode(store: Store, lastTransition: Option[Transition]) extends Print {
    lastTransition match {
      case Some(transition) =>
        transition.command match {
          case _: Command =>
          case _ => throw new Exception
        }
      case None =>
    }

    override def print(): String = {
      lastTransition match {
        case Some(lastTransition) =>
          s"${lastTransition.toString} ==> ${store.toString}"
        case None => store.toString
      }
    }
  }

  case class Trace(nodes: List[TraceNode]) extends Print {
    lazy val costTraceAssociation: CostTraceAssociation = {
      val indexMap =
        nodes.zipWithIndex.foldLeft(Map(): Map[Int, CostTraceNode])({
          case (indexMap, (node, index)) => node.lastTransition match {
            case Some(lastTransition) =>
              (lastTransition.cost, lastTransition.command) match {
                case (Some(cost), use: Use) =>
                  val node = UseNode(use, cost)
                  indexMap + (index -> node)
                case _ => indexMap
              }
            case None => indexMap
          }
        })
      CostTraceAssociation(this, indexMap)
    }
    lazy val costTrace: CostTrace = CostTrace(costTraceAssociation.costTrace(indices = None))

    def add(node: TraceNode): Trace = Trace(nodes :+ node)

    override def print(): String = {
      val prefix = "Trace: "
      val linePrefix: String = " " * prefix.length
      val string = printNodes(nodes.map(n => s"[${n.print()}]"), 1, linePrefix, "==>")
      s"$prefix$string"
    }

    lazy val variables: List[(String, BrboType.T)] = {
      val variables: Map[String, BrboType.T] = nodes.map(node => node.store.getVariables).foldLeft(Map(): Map[String, BrboType.T])({
        case (soFar, map) => soFar ++ map
      })
      variables.toList.sortWith({
        case ((name1, type1), (name2, type2)) => name1 < name2 && type1.toString < type2.toString
      })
    }

    def toTable(printStores: Boolean, onlyGhostCommand: Boolean,
                omitExpressions: Boolean = true, commandMaxLength: Int = 30): (Table, Map[Int, Int]) = {
      val table: Table = Table.create("")
      // From the indices in the shortened table to the actual indices
      var indexMap: Map[Int, Int] = Map()
      val commands: ArrayBuffer[String] = ArrayBuffer()
      val costs: ArrayBuffer[String] = ArrayBuffer()
      val actualIndices: ArrayBuffer[Int] = ArrayBuffer()
      var values: Map[String, ArrayBuffer[String]] = Map()
      nodes.zipWithIndex.foreach({
        case (TraceNode(store, lastTransition), actualIndex) =>
          lastTransition match {
            case Some(transition) =>
              if (omitExpressions && transition.command.isInstanceOf[BrboExpr]) ()
              else {
                if (transition.command.isInstanceOf[GhostCommand] || !onlyGhostCommand) {
                  indexMap = indexMap + (commands.length -> actualIndex)
                  val (commandString, costString) = transition.print(onlyGhostCommand, commandMaxLength)
                  commands.append(commandString)
                  costs.append(costString)
                  actualIndices.append(actualIndex)
                  if (printStores) {
                    variables.foreach({
                      case (name, _) =>
                        val value = store.printValue(name)
                        values.get(name) match {
                          case Some(array) => array.append(value)
                          case None =>
                            val array = ArrayBuffer[String]()
                            array.append(value)
                            values = values + (name -> array)
                        }
                    })
                  }
                }
              }
            case _ =>
          }
      })
      // table.addColumns(IntColumn.create("Index", Range(0, commands.length): _*))
      table.addColumns(IntColumn.create("Index", actualIndices: _*))
      table.addColumns(StringColumn.create("Commands", commands: _*))
      table.addColumns(StringColumn.create("Costs", costs: _*))
      values.toList.sortWith({ case (p1, p2) => p1._1 < p2._1 }).foreach({
        case (identifier, values) =>
          table.addColumns(StringColumn.create(identifier, values: _*))
      })
      (table, indexMap)
    }
  }

  object EmptyTrace extends Trace(Nil) {
    override def print(): String = "EmptyTrace"
  }

  abstract class CostTraceNode extends Print {
    def getGhostCommand: GhostCommand
  }

  case class UseNode(use: Use, cost: Int) extends CostTraceNode {
    override def print(): String = {
      s"${use.printToIR()} (cost=$cost)"
    }

    override def getGhostCommand: GhostCommand = use
  }

  case class CostTrace(nodes: List[CostTraceNode]) extends Print {
    override def print(): String = {
      val prefix = "Use Trace: "
      val linePrefix: String = " " * prefix.length
      val string = printNodes(nodes.map(n => n.print()), 1, linePrefix, "->")
      s"$prefix$string"
    }
  }

  /**
   *
   * @param trace    The trace that this cost trace is associated with
   * @param indexMap A map from indices of the trace to the cost trace nodes
   */
  case class CostTraceAssociation(trace: Trace,
                                  indexMap: Map[Int, CostTraceNode]) {
    private val subsequence = indexMap.toList.sortWith({
      case ((index1, _), (index2, _)) => index1 < index2
    })

    def costTrace(indices: Option[List[Int]]): List[CostTraceNode] = {
      val trace = indices match {
        case Some(indices) => subsequence.filter({ case (index, _) => indices.contains(index) })
        case None => subsequence
      }
      trace.map({ case (_, node) => node })
    }

    def ghostCommandAtIndex(index: Int): GhostCommand = indexMap(index).getGhostCommand

    def costSumAtIndices(indices: List[Int]): Int = {
      indexMap.filter({
        case (index, _) => indices.contains(index)
      }).map({
        case (_, UseNode(_, cost)) => cost
        case _ => 0
      }).sum
    }
  }

  def printState(state: State): String = {
    state match {
      case state: FlowBeginState =>
        s"${state.getClass.getSimpleName}\nCommand: ${state.ast}\n${state.store}\n${state.trace.print()}"
      case state: GoodState =>
        val valueString = state.value match {
          case Some(value) => s"Some(${value.printToIR()})"
          case None => "None"
        }
        s"${state.getClass.getSimpleName}\nValue: $valueString\n${state.store}\n${state.trace.print()}"
      case state: JumpState =>
        s"${state.getClass.getSimpleName}\n${state.store}\n${state.trace.print()}"
      case _ => throw new Exception
    }
  }

  private def printNodes(nodes: List[String], nodesPerLine: Int, linePrefix: String, arrow: String): String = {
    nodes.grouped(nodesPerLine)
      .map(group => group.mkString(s" $arrow "))
      .mkString(s"\n$linePrefix")
  }
}