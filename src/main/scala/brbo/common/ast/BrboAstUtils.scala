package brbo.common.ast

object BrboAstUtils {
  def collectCommands(brboAst: BrboAst): Set[Command] = {
    brboAst match {
      case command: Command => Set(command)
      case statement: Statement =>
        statement match {
          case Block(asts, _) => asts.flatMap(ast => collectCommands(ast)).toSet
          case ITE(_, thenAst, elseAst, _) => collectCommands(thenAst) ++ collectCommands(elseAst)
          case Loop(_, loopBody, _) => collectCommands(loopBody)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  def collectUseDefVariables(brboAst: BrboAst): Set[Identifier] = {
    brboAst match {
      case command: Command => command.getDefs ++ command.getUses
      case statement: Statement =>
        statement match {
          case Block(asts, _) => asts.flatMap(ast => collectUseDefVariables(ast)).toSet
          case ITE(condition, thenAst, elseAst, _) =>
            condition.getUses ++ condition.getDefs ++ collectUseDefVariables(thenAst) ++ collectUseDefVariables(elseAst)
          case Loop(condition, loopBody, _) =>
            condition.getUses ++ condition.getDefs ++ collectUseDefVariables(loopBody)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  def replaceAst(body: BrboAst, oldAst: BrboAst, newAst: BrboAst): BrboAst = {
    if (body == oldAst) return newAst
    body match {
      case command: Command => command
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            Block(asts.map(ast => replaceAst(ast, oldAst, newAst)))
          case ITE(condition, thenAst, elseAst, _) =>
            ITE(condition, replaceAst(thenAst, oldAst, newAst), replaceAst(elseAst, oldAst, newAst))
          case Loop(condition, loopBody, _) =>
            Loop(condition, replaceAst(loopBody, oldAst, newAst))
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  def findParentStatements(currentNode: BrboAstNode, parent: Option[Statement] = None): Map[BrboAstNode, Statement] = {
    val currentMap: Map[BrboAstNode, Statement] = parent match {
      case Some(value) => Map(currentNode -> value)
      case None => Map()
    }
    val childrenMap: Map[BrboAstNode, Statement] = currentNode match {
      case brboAst: BrboAst =>
        brboAst match {
          case _: Command => Map()
          case statement: Statement =>
            statement match {
              case Block(asts, _) =>
                asts.flatMap(ast => findParentStatements(ast, Some(statement))).toMap
              case ITE(condition, thenAst, elseAst, _) =>
                findParentStatements(condition, Some(statement)) ++ findParentStatements(thenAst, Some(statement)) ++
                  findParentStatements(elseAst, Some(statement))
              case Loop(condition, loopBody, _) =>
                findParentStatements(condition, Some(statement)) ++ findParentStatements(loopBody, Some(statement))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
      case _: BrboExpr => Map()
    }
    currentMap ++ childrenMap
  }
}
