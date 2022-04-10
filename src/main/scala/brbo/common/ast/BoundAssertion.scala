package brbo.common.ast

import brbo.common.{BrboType, GhostVariableTyp, GhostVariableUtils, SameAs}

/**
 *
 * @param resourceVariable The resource variable used in the assertion
 * @param assertion        The assertion about the resource variable
 */
case class BoundAssertion(resourceVariable: String, assertion: BrboExpr, tag: String) extends SameAs with Serializable {
  def replaceResourceVariable(into: BrboExpr): BrboExpr = {
    BrboExprUtils.replaceCLiteral(assertion, Identifier(resourceVariable, BrboType.INT), into)
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case BoundAssertion(otherResourceVariable, otherAssertion, otherTag) =>
        otherResourceVariable == resourceVariable && otherAssertion.sameAs(assertion) && otherTag == tag
      case _ => false
    }
  }
}

object BoundAssertion {
  def parse(tag: BrboExpr, expr: BrboExpr): BoundAssertion = {
    val allResourceIdentifiers = BrboExprUtils.collectIdentifiers(expr).filter(i => GhostVariableUtils.isGhostVariable(i.name, GhostVariableTyp.Resource))
    if (allResourceIdentifiers.isEmpty || allResourceIdentifiers.size > 1) {
      throw new Exception(s"Expect using exactly one resource variable in `$expr`. Found the following resource variables instead: `$allResourceIdentifiers`.")
    }
    else {
      tag match {
        case StringLiteral(value, _) =>
          BoundAssertion(allResourceIdentifiers.head.name, expr, value)
        case _ => throw new Exception(s"Expect the tag of the bound assertion to be a string literal: `$tag`.")
      }
    }
  }
}