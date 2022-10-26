package brbo.backend.interpreter

import brbo.backend.interpreter.Interpreter._
import brbo.common.MyLogger
import brbo.common.ast.BrboAstUtils.{immediateOuterLoop, immediateParentStatements, nextAst}
import brbo.common.ast._

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

class Interpreter(brboProgram: BrboProgram, debugMode: Boolean = false) {
  protected val logger: MyLogger = MyLogger.createLogger(classOf[Interpreter], debugMode)
  private val parentStatements: Map[BrboAst, Statement] =
    (brboProgram.mainFunction :: brboProgram.functions).flatMap(f => immediateParentStatements(f.bodyWithInitialization)).toMap

  def execute(inputValues: List[BrboValue]): FlowEndState =
    evaluateFunction(brboProgram.mainFunction, inputValues, EmptyTrace)

  def evaluateFunction(brboFunction: BrboFunction, inputValues: List[BrboValue], lastTrace: Trace): FlowEndState = {
    val parameters = brboFunction.parameters
    assert(parameters.length == inputValues.length)
    val initialStore = parameters.zip(inputValues).foldLeft(new Store())({
      case (store, (parameter, inputValue)) => store.set(parameter, inputValue)
    })
    logger.traceOrError(s"Evaluate function `${brboFunction.identifier}` with initial store $initialStore")
    val initialState = InitialState(brboFunction.bodyWithInitialization, initialStore, lastTrace.add(TraceNode(initialStore, None)))
    val finalState = evaluateAst(initialState)
    // TODO: Sanity check
    finalState
  }

  def evaluateAst(initialState: InitialState): FlowEndState = {
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
                  val lastAst = asts(i - 1)
                  // logger.traceOrError(s"${lastAst.printToC(0)} ${printState(state)}")
                  val loop = immediateOuterLoop(lastAst, parentStatements) match {
                    case Some(loop) => loop
                    case None => throw new Exception
                  }
                  jump match {
                    case Interpreter.BreakJump =>
                      nextAst(loop, parentStatements) match {
                        case Some(nextAst) =>
                          // logger.traceOrError(s"Next ast: ${nextAst.printToC(0)}")
                          return evaluateAst(InitialState(nextAst, store, trace))
                        case None => GoodState(store, trace, None)
                      }
                    case Interpreter.ContinueJump =>
                      // logger.traceOrError(s"Next ast: ${loop.printToC(0)}")
                      return evaluateAst(InitialState(loop, store, trace))
                  }
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
                    val block = Block(List(loopBody, loop))
                    if (b) evaluateAst(InitialState(block, store, trace))
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
    val ast = initialState.ast
    ast match {
      case _: CFGOnly | _: CexPathOnly => throw new Exception()
      case _: VariableDeclaration | _: Assignment =>
        evaluateAssignment(initialState, ast)
      case _: Skip => GoodState(initialState.store, appendToTraceFrom(initialState, Transition(ast)), None)
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
                      val cost = value.get match {
                        case Number(n, _) => Some(n)
                        case _ => throw new Exception
                      }
                      val newStore = store.set(use.resourceVariable, value.get)
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
              case PreDefinedFunctions.VerifierNondetInt.name =>
                val random = new scala.util.Random
                GoodState(initialState.store, appendToTraceFrom(initialState, lastTransition), Some(Number(random.nextInt())))
              case PreDefinedFunctions.BoundAssertion.name => throw new Exception
              case PreDefinedFunctions.Uninitialize.name => throw new Exception
              case _ =>
                evaluateFunctionCall(initialState, specialFunction.internalRepresentation, arguments)
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
          case GoodState(store, trace, value) =>
            value match {
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
      case ArrayLength(array, _) => ???
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
    logger.traceOrError(s"Evaluate function call: `${function.identifier}` with arguments $arguments")
    val (state, argumentValues) = arguments.foldLeft(
      GoodState(initialState.store, initialState.trace, None): FlowEndState,
      Nil: List[BrboValue]
    )({
      case ((state, argumentValues), argument) =>
        state match {
          case GoodState(store, trace, _) =>
            evaluateExpr(InitialState(argument, store, trace)) match {
              case goodState@GoodState(_, _, value) =>
                logger.traceOrError(s"Actual argument `$argument` is evaluated as `$value`")
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
  class Store {
    private var map = new HashMap[String, BrboValue]

    def set(variable: Identifier, value: BrboValue): Store = {
      val store = new Store
      store.map = map + (variable.name -> value)
      store
    }

    def get(variable: Identifier): BrboValue = map.getOrElse(variable.name,
      throw new Exception(s"Variable `$variable` is not defined in ${this.toString}"))

    override def toString: String = {
      val values = map.map({
        case (identifier, value) => s"$identifier -> $value"
      }).toList.sorted.mkString(", ")
      s"Store: ($values)"
    }
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
  abstract class FlowBeginState(val store: Store,
                                val trace: Trace) extends State

  case class GoodState(override val store: Store,
                       override val trace: Trace,
                       value: Option[BrboValue]) extends FlowEndState(store, trace)

  sealed trait Jump

  object BreakJump extends Jump

  object ContinueJump extends Jump

  case class JumpState(override val store: Store,
                       override val trace: Trace,
                       jump: Jump) extends FlowEndState(store, trace)

  case class InitialState(ast: BrboAst,
                          override val store: Store,
                          override val trace: Trace) extends FlowBeginState(store, trace)

  class BadStateException(val store: Store, val trace: Trace) extends Exception

  /**
   *
   * @param store          The current store
   * @param lastTransition The last transition that was evaluated
   */
  case class TraceNode(store: Store, lastTransition: Option[Transition]) {
    lastTransition match {
      case Some(transition) =>
        transition.command match {
          case _: Command =>
          case _ => throw new Exception
        }
      case None =>
    }

    override def toString: String = {
      lastTransition match {
        case Some(lastTransition) =>
          s"${lastTransition.toString} ==> ${store.toString}"
        case None => store.toString
      }
    }
  }

  case class UseNode(use: Use, cost: Int)

  case class Trace(nodes: List[TraceNode]) {
    // val useTrace: List[UseNode] = ???

    def add(node: TraceNode): Trace = Trace(nodes :+ node)

    override def toString: String = {
      val indent: String = " " * "Trace: ".length
      val string =
        nodes.map(n => s"[${n.toString}]")
          .grouped(1) // How many nodes per line
          .map(group => group.mkString(" ==> "))
          .mkString(s"\n$indent")
      s"Trace: $string"
    }
  }

  object EmptyTrace extends Trace(Nil) {
    override def toString: String = "EmptyTrace"
  }

  def printState(state: State): String = {
    state match {
      case state: InitialState =>
        s"${InitialState.getClass.getSimpleName}\nCommand: ${state.ast}\n${state.store}\n${state.trace.toString}"
      case state: GoodState =>
        s"${GoodState.getClass.getSimpleName}\nValue: ${state.value}\n${state.store}\n${state.trace.toString}"
      case state: JumpState =>
        s"${JumpState.getClass.getSimpleName}\n${state.store}\n${state.trace.toString}"
      case _ => throw new Exception
    }
  }
}