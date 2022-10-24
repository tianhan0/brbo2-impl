package brbo.backend.interpreter

import brbo.backend.interpreter.Interpreter._
import brbo.common.ast._

import scala.collection.immutable.HashMap

class Interpreter(brboProgram: BrboProgram) {
  def interpret(inputValues: List[BrboValue]): TerminalState =
    evaluateFunction(brboProgram.mainFunction, inputValues, EmptyTrace, None)

  def evaluateFunction(brboFunction: BrboFunction, inputValues: List[BrboValue],
                       lastTrace: Trace, lastTransition: Option[LastTransition]): TerminalState = {
    val parameters = brboFunction.parameters
    assert(parameters.length == inputValues.length)
    val initialStore = new Store()
    parameters.zip(inputValues).foreach({
      case (parameter, inputValue) => initialStore.set(parameter, inputValue)
    })
    val initialState = InitialState(brboFunction.actualBody, initialStore,
      lastTrace.add(TraceNode(initialStore, lastTransition)), lastTransition)
    val finalState = evaluateAst(initialState)
    // TODO: Sanity check
    finalState
  }

  def evaluateAst(state: InitialState): TerminalState = {
    state.ast match {
      case _: BrboExpr => evaluateExpr(state)
      case _: Command => evaluateCommand(state)
      case statement: Statement =>
        statement match {
          case Block(asts, _) => ???
          case ITE(condition, thenAst, elseAst, _) => ???
          case Loop(condition, loopBody, _) => ???
          case _ => ???
        }
      case _ => throw new Exception()
    }
  }

  def evaluateExpr(initialState: InitialState): TerminalState = {
    val ast = initialState.ast
    ast match {
      case brboValue: BrboValue =>
        val lastTransition = LastTransition(ast)
        GoodState(initialState.store, traceAppend(initialState, lastTransition), Some(brboValue), Some(lastTransition))
      case identifier@Identifier(_, _, _) =>
        val lastTransition = LastTransition(ast)
        GoodState(initialState.store, traceAppend(initialState, lastTransition), Some(initialState.store.get(identifier)), Some(lastTransition))
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
        val newState = evaluateExpr(InitialState(expression, initialState.store, initialState.trace, initialState.lastTransition))
        newState match {
          case BadState(_, _, _) => newState
          case GoodState(store, trace, Some(Bool(b, _)), _) =>
            GoodState(store, trace, Some(Bool(!b)), Some(LastTransition(ast)))
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
        val lastTransition = LastTransition(ast)
        PreDefinedFunctions.specialFunctions.find(f => f.name == identifier) match {
          case Some(specialFunction) =>
            specialFunction.name match {
              case PreDefinedFunctions.VerifierError.name | PreDefinedFunctions.Abort.name =>
                BadState(initialState.store, initialState.trace, Some(lastTransition))
              case PreDefinedFunctions.VerifierNondetInt.name =>
                val random = new scala.util.Random
                GoodState(initialState.store, initialState.trace, Some(Number(random.nextInt())), Some(lastTransition))
              case PreDefinedFunctions.BoundAssertion.name => throw new Exception
              case PreDefinedFunctions.Uninitialize.name => throw new Exception
              case _ =>
                evaluateFunctionCall(ast, initialState.trace, specialFunction.internalRepresentation, arguments)
            }
          case None =>
            brboProgram.functions.find(f => f.identifier == identifier) match {
              case Some(function) => evaluateFunctionCall(ast, initialState.trace, function, arguments)
              case None => throw new Exception(s"Evaluating a function that is not defined: $ast")
            }
        }
      case ITEExpr(condition, thenExpr, elseExpr, _) =>
        val newState = evaluateExpr(InitialState(condition, initialState.store, initialState.trace, initialState.lastTransition))
        newState match {
          case BadState(_, _, _) => newState
          case GoodState(store, trace, value, lastTransition) =>
            value match {
              case Some(Bool(b, _)) =>
                if (b) evaluateExpr(InitialState(thenExpr, store, trace, lastTransition))
                else evaluateExpr(InitialState(elseExpr, store, trace, lastTransition))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
    }
  }

  private def evaluateCommand(state: InitialState): TerminalState = {
    ???
  }

  private def evaluateBinaryExpr(thisExpr: BrboAst, left: BrboExpr, right: BrboExpr, initialState: InitialState,
                                 composeValues: (BrboValue, BrboValue) => BrboValue): TerminalState = {
    val leftState = evaluateExpr(InitialState(left, initialState.store, initialState.trace, initialState.lastTransition))
    val rightState = evaluateExpr(InitialState(right, leftState.store, leftState.trace, leftState.lastTransition))
    (leftState, rightState) match {
      case (GoodState(_, _, Some(leftValue), _), GoodState(_, _, Some(rightValue), _)) =>
        val lastTransition = LastTransition(thisExpr)
        GoodState(
          rightState.store,
          traceAppend(rightState, lastTransition),
          Some(composeValues(leftValue, rightValue)),
          Some(lastTransition)
        )
      case (BadState(_, _, _), _) => leftState
      case (_, BadState(_, _, _)) => rightState
      case _ => throw new Exception
    }
  }

  private def evaluateFunctionCall(callExpr: BrboAst, lastTrace: Trace,
                                   function: BrboFunction, arguments: List[BrboExpr]): TerminalState = {
    evaluateFunction(function, arguments.map(e => e.asInstanceOf[BrboValue]),
      lastTrace, Some(LastTransition(callExpr)))
  }

  private def traceAppend(lastState: State, lastTransition: LastTransition): Trace = {
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

    def get(variable: Identifier): BrboValue = map.getOrElse(variable, throw new Exception)

    override def toString: String = map.toString()
  }

  /**
   *
   * @param command The last command or expression that was evaluated
   * @param cost    The cost (if any) from the last command
   */
  case class LastTransition(command: BrboAst, cost: Option[Int] = None) {
    override def toString: String = {
      val commandString = command.printToC(0)
      s"($commandString, $cost)"
    }
  }

  abstract class State

  /**
   *
   * @param store          The store where the execution terminates
   * @param trace          The trace from the initial state to the current (terminal) state
   * @param lastTransition The last transition that was executed before reaching the current state
   */
  abstract class TerminalState(val store: Store,
                               val trace: Trace,
                               val lastTransition: Option[LastTransition]) extends State

  /**
   *
   * @param store          The store where the execution begins
   * @param trace          The trace from the initial state to the current (non-terminal) state
   * @param lastTransition The last transition that was executed before reaching the current state
   */
  abstract class NonTerminalState(val store: Store,
                                  val trace: Trace,
                                  val lastTransition: Option[LastTransition]) extends State

  case class BadState(override val store: Store,
                      override val trace: Trace,
                      override val lastTransition: Option[LastTransition]) extends TerminalState(store, trace, lastTransition)

  case class GoodState(override val store: Store,
                       override val trace: Trace,
                       value: Option[BrboValue],
                       override val lastTransition: Option[LastTransition]) extends TerminalState(store, trace, lastTransition)

  case class InitialState(ast: BrboAst,
                          override val store: Store,
                          override val trace: Trace,
                          override val lastTransition: Option[LastTransition]) extends NonTerminalState(store, trace, lastTransition)

  /**
   *
   * @param store          The current store
   * @param lastTransition The last transition that was evaluated
   */
  case class TraceNode(store: Store, lastTransition: Option[LastTransition]) {
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
          s"${lastTransition.toString}->${store.toString}"
        case None => store.toString
      }
    }
  }

  case class UseNode(use: Use, cost: Int)

  case class Trace(nodes: List[TraceNode]) {
    // val useTrace: List[UseNode] = ???

    def add(node: TraceNode): Trace = Trace(nodes :+ node)

    override def toString: String = {
      nodes.map(n => n.toString).mkString("->")
    }
  }

  object EmptyTrace extends Trace(Nil) {
    override def toString: String = "EmptyTrace"
  }

  def printState(state: State): String = {
    state match {
      case state: InitialState =>
        s"Command: ${state.ast}\nStore: ${state.store}\nTrace: ${state.trace.toString}\nLast Transition: ${state.lastTransition}"
      case state: GoodState =>
        s"Value: ${state.value}\nStore: ${state.store}\nTrace: ${state.trace.toString}\nLast Transition: ${state.lastTransition}"
      case state: BadState =>
        s"Store: ${state.store}\nTrace: ${state.trace.toString}\nLast Transition: ${state.lastTransition}"
      case _ => throw new Exception
    }
  }
}