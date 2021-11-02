package brbo.backend.refiner

import brbo.common.ast._
import brbo.common.{MathUtils, Z3Solver}
import com.microsoft.z3.AST

case class Predicate(expr: BrboExpr) {
  override def toString: String = expr.prettyPrintToCFG

  def toAst(solver: Z3Solver): AST = expr.toZ3AST(solver)
}

object Predicate {
  private val CONSTANTS = Set(Number(0)) //, Number(1))

  def generatePredicates(variables: Set[Identifier], relational: Boolean): List[Predicate] = {
    val intervalPredicates: Set[Predicate] = variables.flatMap({
      v =>
        val expressions = Set(v, Subtraction(Number(0), v))
        expressions.flatMap({
          expression =>
            CONSTANTS.flatMap({
              constant =>
                val ge = Predicate(GreaterThanOrEqualTo(expression, constant))
                val gt = Predicate(GreaterThan(expression, constant))
                List(ge, gt)
            })
        })
    })
    val octagonPredicates: List[Predicate] = MathUtils.choose2(variables).flatMap({
      case (v1, v2) =>
        val expressions = Set(Addition(v1, v2), Subtraction(v1, v2), Subtraction(v2, v1), Subtraction(Subtraction(Number(0), v1), v2))
        expressions.flatMap({
          expression =>
            CONSTANTS.map(constant =>
              Predicate(LessThanOrEqualTo(expression, constant))
            )
        })
    })
    val result = {
      if (relational) intervalPredicates ++ octagonPredicates
      else intervalPredicates
    }
    val conjunct2 = MathUtils.choose2(result).map({ case (p1, p2) => Predicate(And(p1.expr, p2.expr)) })
    val TRUE = Predicate(Bool(b = true))
    TRUE :: sortPredicates(conjunct2) ::: sortPredicates(result)
  }

  private def sortPredicates(iterable: Iterable[Predicate]): List[Predicate] = iterable.toList.sortWith({ case (p1, p2) => p1.toString < p2.toString })
}
