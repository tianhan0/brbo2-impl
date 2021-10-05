package brbo.common.ast

import brbo.common.Z3Solver
import com.microsoft.z3.AST

trait ToZ3AST {
  def toZ3AST(solver: Z3Solver): AST
}
