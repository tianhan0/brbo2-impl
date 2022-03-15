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
                    interpreterKind: InterpreterKind, arguments: CommandLineArguments): (AST, Set[String]) = {
    interpreterKind match {
      case brbo.backend.verifier.InterpreterKind.SYMBOLIC_EXECUTION =>
        val symbolicExecution = new SymbolicExecution(inputVariables, arguments.getDebugMode)
        val SymbolicExecution.Result(finalState, declaredVariables) = symbolicExecution.execute(path, solver)
        val finalStateAst = solver.mkAnd(valuationToAST(finalState.valuations.head, solver), finalState.pathCondition)
        (finalStateAst, declaredVariables)
      case brbo.backend.verifier.InterpreterKind.MODEL_CHECK =>
        val mainFunction = BrboFunction("main", BrboType.VOID, inputVariables, Block(path.map({
          n =>
            // Give every node a new ID. Otherwise if a node appears twice
            // in the given path, the generated CFG will contain a loop.
            n.value match {
              case expr: BrboExpr => Assume(expr)
              case command: Command => BrboAstUtils.generateNewId(command)
              case _ => throw new Exception
            }
        })), groupIds = Set())
        val brboProgram = BrboProgram("Symbolic Execution", mainFunction = mainFunction)
        val abstractMachine = new AbstractMachine(brboProgram, arguments)
        val AbstractMachine.Result(_, finalValuations) = abstractMachine.verify(Bool(b = true), maxPathLength = 10000)
        assert(finalValuations.size == 1, s"finalValuations: ${finalValuations.map(s => s.toShortString)}")
        val finalValuation = finalValuations.head
        val ast = finalValuation.stateToZ3Ast(solver, toInt = true)
        (ast, finalValuation.allVariablesNoScope.map(v => v.name).toSet)
    }
  }

  /**
   *
   * @param solver The z3 solver used for representing input variables
   * @return The input variables of the program to which this abstract interpreter is applied
   */
  def getInputVariables(inputVariables: Iterable[Identifier], solver: Z3Solver): Set[AST] = {
    inputVariables.map({ parameter => Z3Solver.variableToZ3(parameter.name, parameter.typ, solver) }).toSet
  }
}
