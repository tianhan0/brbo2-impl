package brbo.backend.verifier

import brbo.backend.verifier.SymbolicExecution._
import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{BOOL, BrboType, INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{BrboType, StringCompare, Z3Solver}
import com.microsoft.z3.{AST, BoolExpr}

import scala.annotation.tailrec

class SymbolicExecution(path: Path, brboProgram: BrboProgram) {
  private val solver = new Z3Solver

  private var inputs: Valuation = brboProgram.mainFunction.parameters.foldLeft(Map[String, (BrboType, Value)]())({
    (acc, parameter) =>
      val z3AST = parameter.typ match {
        case INT => solver.mkIntVar(parameter.identifier)
        case BOOL => solver.mkBoolVar(parameter.identifier)
        case VOID => throw new Exception
      }
      acc + (parameter.identifier -> (parameter.typ, Value(z3AST)))
  })

  def createFreshVariable(typ: BrboType): AST = {
    val variableName = s"v${inputs.size}"
    val z3AST = typ match {
      case INT => solver.mkIntVar(variableName)
      case BOOL => solver.mkBoolVar(variableName)
      case VOID => throw new Exception
    }
    inputs = inputs + (variableName -> (typ, Value(z3AST)))
    z3AST
  }

  val result: State = path.pathNodes.foldLeft(State(List(inputs), solver.mkTrue(), Map()))({ (acc, node) => evaluate(acc, node) })

  @tailrec
  private def evaluate(state: State, node: CFGNode): State = {
    val valuation: Valuation = state.valuations.head

    node.value match {
      case Left(command) =>
        command match {
          case CallFunction(callee, actualArguments) => // Calling a new function
            assert(callee.parameters.length == actualArguments.length)
            // Assume that when calling a function, all actual arguments must have a value
            val (reversedValues: List[AST], newReturnValues) =
              actualArguments.foldLeft((Nil: List[AST], state.returnValues))({
                case ((values, returnValues), actualArgument) =>
                  val (value, newReturnValue) = evaluateExpression(valuation, returnValues, actualArgument)
                  (value.get :: values, newReturnValue)
              })
            val map =
              callee.parameters.zip(reversedValues.reverse).foldLeft(Map[String, (BrboType, Value)]())({
                case (acc, (formalArgument, value)) =>
                  acc + (formalArgument.identifier -> (formalArgument.typ, Value(value)))
              })
            State(map :: state.valuations, state.pathCondition, newReturnValues)
          case Return(value, _) =>
            val newReturnValues: ReturnValues =
              value match {
                case Some(value2) =>
                  val (returnValue, newReturnValues) = evaluateExpression(valuation, state.returnValues, value2)
                  val currentFunctionName = node.function.identifier
                  val newList: List[Value] =
                    state.returnValues.get(currentFunctionName) match {
                      case Some(list) => Value(returnValue.get) :: list
                      case None => List(Value(returnValue.get))
                    }
                  // Push the return value
                  newReturnValues.updated(currentFunctionName, newList)
                case None => state.returnValues
              }
            // Pop the environment map
            State(state.valuations.tail, state.pathCondition, newReturnValues)
          case Assignment(_, _, _) | VariableDeclaration(_, _, _) =>
            val (newValuation, newReturnValues) = {
              val command2 = command match {
                case x@Assignment(_, _, _) => Left(x)
                case x@VariableDeclaration(_, _, _) => Right(x)
              }
              evaluateAssignment(valuation, state.returnValues, command2)
            }
            // Update the environment map
            State(newValuation :: state.valuations.tail, state.pathCondition, newReturnValues)
          case FunctionExit(_) =>
            // Pop the environment map
            State(state.valuations.tail, state.pathCondition, state.returnValues)
          case LabeledCommand(_, command2, _) =>
            evaluate(state, CFGNode(Left(command2), node.function, CFGNode.DONT_CARE_ID))
          case FunctionCall(functionCallExpr, _) =>
            val (_, newReturnValues) = evaluateExpression(valuation, state.returnValues, functionCallExpr)
            State(state.valuations, state.pathCondition, newReturnValues)
          case use@Use(_, _, _) =>
            val (newValuation, newReturnValues) = evaluateAssignment(valuation, state.returnValues, Left(use.assignmentCommand))
            State(newValuation :: state.valuations.tail, state.pathCondition, newReturnValues)
          case reset@Reset(_, _) =>
            var newValuation: Valuation = valuation
            var newReturnValues: ReturnValues = state.returnValues
            List(reset.maxCommand, reset.resetCommand, reset.counterCommand).foreach({
              assignment =>
                val r = evaluateAssignment(newValuation, newReturnValues, Left(assignment))
                newValuation = r._1
                newReturnValues = r._2
            })
            State(newValuation :: state.valuations.tail, state.pathCondition, newReturnValues)
          case _: CFGOnly | Skip(_) | Break(_) | Continue(_) => throw new Exception(s"Unexpected command: `$command`")
        }
      case Right(brboExpr) =>
        val (additionPathCondition, newReturnValues) = evaluateExpression(valuation, state.returnValues, brboExpr)
        State(state.valuations, solver.mkAnd(state.pathCondition, additionPathCondition.get), newReturnValues)
    }
  }

  private def evaluateAssignment(valuation: Valuation, returnValues: ReturnValues,
                                 command: Either[Assignment, VariableDeclaration]): (Valuation, ReturnValues) = {
    command match {
      case Left(Assignment(variable, expression, _)) =>
        val (value, newReturnValues) = evaluateExpression(valuation, returnValues, expression)
        (valuation.updated(variable.identifier, (variable.typ, Value(value.get))), newReturnValues)
      case Right(VariableDeclaration(variable, initialValue, _)) =>
        val (value, newReturnValues) = evaluateExpression(valuation, returnValues, initialValue)
        // Check variable name shadowing
        assert(!valuation.contains(variable.identifier), s"Variable `${variable.identifier}` is defined again by `$command`")
        (valuation.updated(variable.identifier, (variable.typ, Value(value.get))), newReturnValues)
    }
  }

  private def evaluateExpression(valuation: Valuation, returnValues: ReturnValues, brboExpr: BrboExpr): (Option[AST], ReturnValues) = {
    brboExpr match {
      case Equal(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkEq(leftValue.get, rightValue.get)), returnValues2)
      case Bool(b, _) => (Some(solver.mkBoolVal(b)), returnValues)
      case Number(n, _) => (Some(solver.mkIntVal(n)), returnValues)
      case Or(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkOr(leftValue.get, rightValue.get)), returnValues2)
      case And(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkAnd(leftValue.get, rightValue.get)), returnValues2)
      case Negative(expression, _) =>
        val (value, returnValues1) = evaluateExpression(valuation, returnValues, expression)
        (Some(solver.mkNot(value.get)), returnValues1)
      case Addition(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkAdd(leftValue.get, rightValue.get)), returnValues2)
      case Subtraction(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkSub(leftValue.get, rightValue.get)), returnValues2)
      case Multiplication(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkMul(leftValue.get, rightValue.get)), returnValues2)
      case Division(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkDiv(leftValue.get, rightValue.get)), returnValues2)
      case LessThan(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkLt(leftValue.get, rightValue.get)), returnValues2)
      case LessThanOrEqualTo(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkLe(leftValue.get, rightValue.get)), returnValues2)
      case GreaterThan(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkGt(leftValue.get, rightValue.get)), returnValues2)
      case GreaterThanOrEqualTo(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkGe(leftValue.get, rightValue.get)), returnValues2)
      case NotEqual(left, right, _) =>
        val (leftValue, returnValues1) = evaluateExpression(valuation, returnValues, left)
        val (rightValue, returnValues2) = evaluateExpression(valuation, returnValues1, right)
        (Some(solver.mkNe(leftValue.get, rightValue.get)), returnValues2)
      case FunctionCallExpr(identifier, _, _, _) =>
        // Assume the return value of this function call is already in the current state
        returnValues.get(identifier) match {
          case Some(list) => (Some(list.head.value), returnValues.updated(identifier, list.tail))
          case None =>
            identifier match {
              case PreDefinedFunctions.VERIFIER_NONDET_INT => (Some(createFreshVariable(INT)), returnValues)
              case PreDefinedFunctions.ABORT => throw new Exception
              case PreDefinedFunctions.VERIFIER_ERROR => (None, returnValues)
              case _ => throw new Exception
            }
        }
      case e@Identifier(identifier, _, _) =>
        valuation.get(identifier) match {
          case Some(value) => (Some(value._2.value), returnValues)
          case None => throw new Exception(s"Find the value of `$e` from `$valuation`")
        }
      case ITEExpr(condition, thenExpr, elseExpr) =>
        val (conditionValue, returnValues1) = evaluateExpression(valuation, returnValues, condition)
        val (thenValue, returnValues2) = evaluateExpression(valuation, returnValues1, thenExpr)
        val (elseValue, returnValues3) = evaluateExpression(valuation, returnValues1, elseExpr)
        assert(returnValues2 == returnValues3)
        (Some(solver.mkITE(conditionValue.get, thenValue.get, elseValue.get)), returnValues2)
    }
  }
}

object SymbolicExecution {
  type ReturnValues = Map[String, List[Value]]

  type Valuation = Map[String, (BrboType, Value)]

  case class Value(value: AST)

  /**
   *
   * @param valuations    A list of stack frames. The newest frame is the head of the list.
   * @param pathCondition The path condition.
   * @param returnValues  A mapping from names of functions (that has been invoked) to their return values. The newest return value is the head of the list.
   */
  case class State(valuations: List[Valuation], pathCondition: BoolExpr, returnValues: ReturnValues) {
    override def toString: String = {
      val valuationsString = s"Valuations:\n  ${valuations.map(valuation => StringCompare.toSortedString(Left(valuation), "\n  ")).mkString("\n----------\n")}"
      val pathConditionString = s"Path condition: $pathCondition"
      val returnValuesString = s"Return values:\n  ${StringCompare.toSortedString(Left(returnValues), "\n  ")}"
      s"$valuationsString\n$pathConditionString\n$returnValuesString"
    }
  }

}