package brbo.backend.refiner

import brbo.backend.verifier.SymbolicExecution.Valuation
import brbo.common.Z3Solver
import brbo.common.ast.BrboExpr
import com.microsoft.z3.AST

case class Predicate(expr: BrboExpr, ast: AST) {
  override def toString: String = expr.prettyPrintToCFG
}

object Predicate {
  def generatePredicates(variables: Valuation, solver: Z3Solver): Set[Predicate] = {
    ???
  }
}
