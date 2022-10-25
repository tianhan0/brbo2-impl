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

  def immediateParentStatements(currentNode: BrboAst, parent: Option[Statement] = None): Map[BrboAst, Statement] = {
    val currentMap: Map[BrboAst, Statement] = parent match {
      case Some(value) => Map(currentNode -> value)
      case None => Map()
    }
    val childrenMap: Map[BrboAst, Statement] = currentNode match {
      case _: Command => Map()
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            asts.flatMap(ast => immediateParentStatements(ast, Some(statement))).toMap
          case ITE(condition, thenAst, elseAst, _) =>
            immediateParentStatements(condition, Some(statement)) ++ immediateParentStatements(thenAst, Some(statement)) ++
              immediateParentStatements(elseAst, Some(statement))
          case Loop(condition, loopBody, _) =>
            immediateParentStatements(condition, Some(statement)) ++ immediateParentStatements(loopBody, Some(statement))
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
    currentMap ++ childrenMap
  }

  def generateNewId(command: Command): Command = {
    command match {
      case BeforeFunctionCall(callee, actualArguments, _) => BeforeFunctionCall(callee, actualArguments)
      case VariableDeclaration(identifier, initialValue, _) => VariableDeclaration(identifier, initialValue)
      case Assignment(identifier, expression, _) => Assignment(identifier, expression)
      case Use(groupId, update, condition, _) => Use(groupId, update, condition)
      case Skip(_) => Skip()
      case Break(_) => Break()
      case Empty(_) => Empty()
      case Continue(_) => Continue()
      case LoopExit(_) => LoopExit()
      case FunctionExit(_) => FunctionExit()
      case BranchingHead(_) => BranchingHead()
      case Return(value, _) => Return(value)
      case Assume(condition, _) => Assume(condition)
      case Reset(groupId, condition, _) => Reset(groupId, condition)
      case LabeledCommand(label, command, _) => LabeledCommand(label, command)
    }
  }
}
