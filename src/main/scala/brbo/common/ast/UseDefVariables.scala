package brbo.common.ast

trait UseDefVariables {
  def getUses: Set[Identifier]

  def getDefs: Set[Identifier]
}
