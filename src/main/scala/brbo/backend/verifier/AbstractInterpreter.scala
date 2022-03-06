package brbo.backend.verifier

import brbo.common.Z3Solver
import brbo.common.cfg.CFGNode
import com.microsoft.z3.AST

abstract class AbstractInterpreter {
  /**
   *
   * @param path   A path
   * @param solver The solver that is used to represent the final state of the path
   * @return The final state of the path, and the set of variable names that have been used
   */
  def interpret(path: List[CFGNode], solver: Z3Solver): (AST, Set[String])

  /**
   *
   * @param solver The z3 solver used for representing input variables
   * @return The input variables of the program to which this abstract interpreter is applied
   */
  def inputs(solver: Z3Solver): Set[AST]
}
