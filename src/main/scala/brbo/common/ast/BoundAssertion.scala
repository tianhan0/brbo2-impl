package brbo.common.ast

import brbo.common.BrboType

/**
 *
 * @param resourceVariable The resource variable used in the assertion
 * @param assertion
 */
case class BoundAssertion(resourceVariable: String, assertion: BrboExpr) {
  def replaceResourceVariable(into: BrboExpr): BrboExpr = {
    BrboExprUtils.replaceCLiteral(assertion, Identifier(resourceVariable, BrboType.INT), into)
  }
}
