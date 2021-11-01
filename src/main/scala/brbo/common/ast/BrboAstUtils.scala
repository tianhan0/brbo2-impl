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

  def replace(body: BrboAst, oldAst: BrboAst, newAst: BrboAst): BrboAst = {
    if (body == oldAst) return newAst
    body match {
      case command: Command => command
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            Block(asts.map(ast => replace(ast, oldAst, newAst)))
          case ITE(condition, thenAst, elseAst, _) =>
            ITE(condition, replace(thenAst, oldAst, newAst), replace(elseAst, oldAst, newAst))
          case Loop(condition, loopBody, _) =>
            Loop(condition, replace(loopBody, oldAst, newAst))
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}
