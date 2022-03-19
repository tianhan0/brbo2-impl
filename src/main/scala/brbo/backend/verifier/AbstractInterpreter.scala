package brbo.backend.verifier

import brbo.backend.verifier.InterpreterKind.InterpreterKind
import brbo.backend.verifier.SymbolicExecution.valuationToAST
import brbo.backend.verifier.modelchecker.AbstractMachine
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import brbo.common.{BrboType, CommandLineArguments, Z3Solver}
import com.microsoft.z3.AST

object AbstractInterpreter {
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
        val abstractMachine = {
          val mainFunction = BrboFunction("main", BrboType.VOID, inputVariables, Block(newPath.reverse), groupIds = Set())
          val brboProgram = BrboProgram("Symbolic Execution", mainFunction = mainFunction)
          new AbstractMachine(brboProgram, arguments)
        }
        val result = abstractMachine.verify(Bool(b = true), maxPathLength = 10000)
        val finalValuation = {
          val finalValuations = result.finalValuations
          assert(finalValuations.size == 1, s"finalValuations: ${finalValuations.map(s => s.toShortString)}")
          finalValuations.head
        }
        val ast = finalValuation.stateToZ3Ast(solver, toInt = true)
        val stateMap = {
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
        Result(ast, finalValuation.allVariablesNoScope.map(v => v.name).toSet, Some(stateMap))
    }
  }

  case class Result(finalState: AST, declaredVariables: Set[String], moreInformation: Option[Any])

  /**
   *
   * @param solver The z3 solver used for representing input variables
   * @return The input variables of the program to which this abstract interpreter is applied
   */
  def getInputVariables(inputVariables: Iterable[Identifier], solver: Z3Solver): Set[AST] = {
    inputVariables.map({ parameter => Z3Solver.variableToZ3(parameter.name, parameter.typ, solver) }).toSet
  }
}
