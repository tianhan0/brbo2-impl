package brbo.backend.interpreter

import brbo.backend.interpreter.Interpreter._
import brbo.common.MyLogger
import brbo.common.ast._

import scala.collection.immutable.HashMap

class Interpreter(brboProgram: BrboProgram, debugMode: Boolean = false) {
  protected val logger: MyLogger = MyLogger.createLogger(classOf[Interpreter], debugMode)

  def execute(inputValues: List[BrboValue]): TerminalState =
    evaluateFunction(brboProgram.mainFunction, inputValues, EmptyTrace)

  def evaluateFunction(brboFunction: BrboFunction, inputValues: List[BrboValue], lastTrace: Trace): TerminalState = {
    val parameters = brboFunction.parameters
    assert(parameters.length == inputValues.length)
    val initialStore = new Store()
    parameters.zip(inputValues).foreach({
      case (parameter, inputValue) => initialStore.set(parameter, inputValue)
    })
    logger.traceOrError(s"Evaluate function `${brboFunction.identifier}` with initial store $initialStore")
    val initialState = InitialState(brboFunction.actualBody, initialStore, lastTrace.add(TraceNode(initialStore, None)))
    val finalState = evaluateAst(initialState)
    // TODO: Sanity check
    finalState
  }

  def evaluateAst(initialState: InitialState): TerminalState = {
    initialState.ast match {
      case _: BrboExpr => evaluateExpr(initialState)
      case _: Command => evaluateCommand(initialState)
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            val state = GoodState(initialState.store, initialState.trace, None)
            asts.foldLeft(state: TerminalState)({
              case (soFar, ast) =>
                soFar match {
                  case BadState(_, _) =>
                    logger.error(s"Error when evaluating statement in a block $ast")
                    soFar
                  case GoodState(store, trace, _) => evaluateAst(InitialState(ast, store, trace))
                  case _ => throw new Exception
                }
            })
          case ITE(condition, thenAst, elseAst, _) =>
            evaluateExpr(InitialState(condition, initialState.store, initialState.trace)) match {
              case badState@BadState(_, _) =>
                logger.error(s"Error when evaluating the conditional in ITE ${initialState.ast}")
                badState
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
              case badState@BadState(_, _) =>
                logger.error(s"Error when evaluating the conditional in loop $loop")
                badState
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
      case _ => throw new Exception()
    }
  }

  def evaluateExpr(initialState: InitialState): TerminalState = {
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
          case BadState(_, _) =>
            logger.error(s"Error when evaluating the expression in `$ast`")
            newState
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
        PreDefinedFunctions.specialFunctions.find(f => f.name == identifier) match {
          case Some(specialFunction) =>
            specialFunction.name match {
              case PreDefinedFunctions.VerifierError.name | PreDefinedFunctions.Abort.name =>
                BadState(initialState.store, appendToTraceFrom(initialState, lastTransition))
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
        val newState = evaluateExpr(InitialState(condition, initialState.store, initialState.trace))
        newState match {
          case BadState(_, _) =>
            logger.error(s"Error when evaluating the conditional in `$ast`")
            newState
          case GoodState(store, _, value) =>
            value match {
              case Some(Bool(b, _)) =>
                val newTrace = appendToTraceFrom(newState, Transition(condition))
                if (b) evaluateExpr(InitialState(thenExpr, store, newTrace))
                else evaluateExpr(InitialState(elseExpr, store, newTrace))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
    }
  }

  private def evaluateCommand(initialState: InitialState): TerminalState = {
    val ast = initialState.ast
    ast match {
      case _: CFGOnly | _: CexPathOnly => throw new Exception()
      case VariableDeclaration(identifier, initialValue, _) => ???
      case Assignment(identifier, expression, _) => ???
      case Use(groupId, update, condition, _) => ???
      case Skip(_) => ???
      case Break(_) => ???
      case Continue(_) => ???
      case Return(expression, _) =>
        val newTrace = appendToTraceFrom(initialState, Transition(ast))
        expression match {
          case Some(expression) =>
            evaluateExpr(InitialState(expression, initialState.store, newTrace))
          case None => GoodState(initialState.store, newTrace, None)
        }
      case Assume(condition, _) => ???
      case Reset(groupId, condition, _) => ???
      case LabeledCommand(label, command, _) => ???
    }
  }

  private def evaluateBinaryExpr(thisExpr: BrboAst, left: BrboExpr, right: BrboExpr, initialState: InitialState,
                                 composeValues: (BrboValue, BrboValue) => BrboValue): TerminalState = {
    val leftState = evaluateExpr(InitialState(left, initialState.store, initialState.trace))
    val rightState = evaluateExpr(InitialState(right, leftState.store, leftState.trace))
    (leftState, rightState) match {
      case (GoodState(_, _, Some(leftValue)), GoodState(_, _, Some(rightValue))) =>
        GoodState(
          rightState.store,
          appendToTraceFrom(rightState, Transition(thisExpr)),
          Some(composeValues(leftValue, rightValue))
        )
      case (BadState(_, _), _) =>
        logger.error(s"Error when evaluating the left expression in `$thisExpr`")
        leftState
      case (_, BadState(_, _)) =>
        logger.error(s"Error when evaluating the right expression in `$thisExpr`")
        rightState
      case _ => throw new Exception
    }
  }

  private def evaluateFunctionCall(initialState: InitialState, function: BrboFunction, arguments: List[BrboExpr]): TerminalState = {
    logger.traceOrError(s"Evaluate function call: `${function.identifier}` with arguments $arguments")
    val (state, argumentValues) = arguments.foldLeft(
      GoodState(initialState.store, initialState.trace, None): TerminalState,
      Nil: List[BrboValue]
    )({
      case ((state, argumentValues), argument) =>
        state match {
          case BadState(_, _) => (state, argumentValues)
          case GoodState(store, trace, _) =>
            evaluateExpr(InitialState(argument, store, trace)) match {
              case badState@BadState(_, _) =>
                logger.error(s"Error when evaluating argument `$argument`")
                (badState, argumentValues)
              case goodState@GoodState(_, _, value) =>
                logger.traceOrError(s"Actual argument `$argument` is evaluated as `$value`")
                (goodState, value.get :: argumentValues)
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
    })
    state match {
      case BadState(_, _) => state
      case GoodState(_, trace, _) =>
        evaluateFunction(function, argumentValues.reverse, trace) match {
          case badState@BadState(_, _) =>
            logger.error(s"Error when evaluating function `${function.identifier}` with arguments $arguments")
            badState
          case GoodState(_, trace, value) =>
            // Restore to the initial store
            GoodState(initialState.store, trace, value)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  private def appendToTraceFrom(lastState: State, lastTransition: Transition): Trace = {
    val (trace, store) = lastState match {
      case state: NonTerminalState => (state.trace, state.store)
      case state: TerminalState => (state.trace, state.store)
      case _ => throw new Exception
    }
    val node = TraceNode(store, Some(lastTransition))
    trace.add(node)
  }
}

object Interpreter {
  class Store {
    private var map = new HashMap[Identifier, BrboValue]

    def set(variable: Identifier, value: BrboValue): Unit = {
      map = map + (variable -> value)
    }

    def get(variable: Identifier): BrboValue = map.getOrElse(variable,
      throw new Exception(s"Variable `$variable` is not defined in ${this.toString}"))

    override def toString: String = {
      val values = map.map({
        case (identifier, value) => s"$identifier -> $value"
      }).mkString(", ")
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
      val commandString = command.printToC(0)
      s"($commandString, $cost)"
    }
  }

  abstract class State

  /**
   *
   * @param store The store where the execution terminates
   * @param trace The trace from the initial state to the current (terminal) state
   */
  abstract class TerminalState(val store: Store,
                               val trace: Trace) extends State

  /**
   *
   * @param store The store where the execution begins
   * @param trace The trace from the initial state to the current (non-terminal) state
   */
  abstract class NonTerminalState(val store: Store,
                                  val trace: Trace) extends State

  case class BadState(override val store: Store,
                      override val trace: Trace) extends TerminalState(store, trace)

  case class GoodState(override val store: Store,
                       override val trace: Trace,
                       value: Option[BrboValue]) extends TerminalState(store, trace)

  case class InitialState(ast: BrboAst,
                          override val store: Store,
                          override val trace: Trace) extends NonTerminalState(store, trace)

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
      val string = nodes.map(n => s"[${n.toString}]").grouped(3).map(group => group.mkString(" ==> ")).mkString(s"\n$indent")
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
      case state: BadState =>
        s"${BadState.getClass.getSimpleName}\n${state.store}\n${state.trace.toString}"
      case _ => throw new Exception
    }
  }
}