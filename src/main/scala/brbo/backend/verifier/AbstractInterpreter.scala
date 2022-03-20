package brbo.backend.verifier

import brbo.backend.verifier.InterpreterKind.InterpreterKind
import brbo.backend.verifier.SymbolicExecution.valuationToAST
import brbo.backend.verifier.modelchecker.AbstractMachine
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{BrboType, CommandLineArguments, Z3Solver}
import com.microsoft.z3.AST

object AbstractInterpreter {
  private val MAX_PATH_LENGTH = 10000

  /**
   *
   * @param path   A path
   * @param solver The solver that is used to represent the final state of the path
   * @return The final state of the path, and the set of variable names that have been used
   */
  def interpretPath(path: List[CFGNode], inputVariables: List[Identifier], solver: Z3Solver,
                    interpreterKind: InterpreterKind, arguments: CommandLineArguments): Result = {
    interpreterKind match {
      case brbo.backend.verifier.InterpreterKind.SYMBOLIC_EXECUTION =>
        val symbolicExecution = new SymbolicExecution(inputVariables, arguments.getDebugMode)
        val SymbolicExecution.Result(finalState, declaredVariables) = symbolicExecution.execute(path, solver)
        val finalStateAst = solver.mkAnd(valuationToAST(finalState.valuations.head, solver), finalState.pathCondition)
        Result(finalStateAst, declaredVariables, None)
      case brbo.backend.verifier.InterpreterKind.MODEL_CHECK =>
        val result = interpretOrVerifyPath(path, None, inputVariables, arguments)
        val finalValuation = {
          val finalValuations = result.result.finalValuations
          assert(finalValuations.size == 1, s"finalValuations: ${finalValuations.map(s => s.toShortString)}")
          finalValuations.head
        }
        val ast = finalValuation.stateToZ3Ast(solver, toInt = true)
        Result(ast, finalValuation.allVariablesNoScope.map(v => v.name).toSet, Some(result.stateMap))
    }
  }

  def verifyPath(path: List[CFGNode], assertion: BrboExpr,
                 inputVariables: List[Identifier], arguments: CommandLineArguments): AbstractMachine.Result = {
    val result = interpretOrVerifyPath(path, Some(assertion), inputVariables, arguments)
    result.result
  }

  private def interpretOrVerifyPath(path: List[CFGNode], assertion: Option[BrboExpr],
                                    inputVariables: List[Identifier], arguments: CommandLineArguments): ModelCheckerResult = {
    val (nodesMap, newPath) = path.zipWithIndex.foldLeft((Map[CommandOrExpr, (CFGNode, Int)](), List[Command]()))({
      case ((accMap, accPath), (node, index)) =>
        // Give every node a new ID. Otherwise if a node appears twice
        // in the given path, the generated CFG will contain a loop.
        val newCommandOrExpr = node.value match {
          case expr: BrboExpr => Assume(expr)
          case command: Command => BrboAstUtils.generateNewId(command)
          case _ => throw new Exception
        }
        (accMap + (newCommandOrExpr -> (node, index)), newCommandOrExpr :: accPath)
    })
    val mainFunction = BrboFunction("main", BrboType.VOID, inputVariables, Block(newPath.reverse), groupIds = Set())
    val brboProgram = BrboProgram("Symbolic Execution", mainFunction = mainFunction)
    val abstractMachine = new AbstractMachine(brboProgram, arguments.copyNoWidening())
    val assertionToVerify =
      assertion match {
        case Some(value) => value
        case None => Bool(b = true)
      }
    val result = abstractMachine.verify(assertionToVerify, MAX_PATH_LENGTH)
    val stateMap: List[AbstractMachine.Valuation] = {
      assert(result.maximalPaths.size == 1)
      val stateMap = result.maximalPaths.head._2.valuations
      stateMap
        .filter({ case (node, _) => nodesMap.contains(node.value) }) // The model checker may generate nodes that do not exist in the given path
        .toList.sortWith({ // Sort the list of command-state pairs by the indices of the command in the given path
        case ((n1, _), (n2, _)) =>
          (nodesMap.get(n1.value), nodesMap.get(n2.value)) match {
            case (Some((_, index1)), Some((_, index2))) => index1 < index2
            case _ => throw new Exception
          }
      }).map({ case (_, valuation) => valuation })
    }
    ModelCheckerResult(result, stateMap)
  }

  case class Result(finalState: AST, declaredVariables: Set[String], moreInformation: Option[Any])

  case class ModelCheckerResult(result: AbstractMachine.Result, stateMap: List[AbstractMachine.Valuation])
}