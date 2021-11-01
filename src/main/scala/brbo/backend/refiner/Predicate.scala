package brbo.backend.refiner

import brbo.common.ast._
import brbo.common.{MathUtils, Z3Solver}
import com.microsoft.z3.AST

case class Predicate(expr: BrboExpr, ast: AST) {
  override def toString: String = expr.prettyPrintToCFG
}

object Predicate {
  private val CONSTANTS = Set(Number(0), Number(1))

  def generatePredicates(variables: Set[Identifier], solver: Z3Solver): List[Predicate] = {
    val intervalPredicates: Set[Predicate] = variables.flatMap({
      v =>
        val expressions = Set(v, Subtraction(Number(0), v))
        expressions.flatMap({
          expression =>
            CONSTANTS.map(constant =>
              Predicate(
                GreaterThanOrEqualTo(expression, constant),
                solver.mkGe(expression.toZ3AST(solver), constant.toZ3AST(solver))
              )
            )
        })
    })
    val octagonPredicates: List[Predicate] = MathUtils.choose2(variables).flatMap({
      case (v1, v2) =>
        val expressions = Set(Addition(v1, v2), Subtraction(v1, v2), Subtraction(v2, v1), Subtraction(Subtraction(Number(0), v1), v2))
        expressions.flatMap({
          expression =>
            CONSTANTS.map(constant =>
              Predicate(
                LessThanOrEqualTo(expression, constant),
                solver.mkLe(expression.toZ3AST(solver), constant.toZ3AST(solver))
              )
            )
        })
    })
    val result = intervalPredicates ++ octagonPredicates
    val TRUE = {
      val t = Bool(b = true)
      Predicate(t, t.toZ3AST(solver))
    }
    TRUE :: result.toList.sortWith({ case (p1, p2) => p1.toString < p2.toString })
  }
}
