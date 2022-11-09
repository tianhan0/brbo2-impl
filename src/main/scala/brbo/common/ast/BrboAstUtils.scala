package brbo.common.ast

import scala.annotation.tailrec

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

  def replaceAst(body: BrboAst, replaces: Map[BrboAst, BrboAst]): BrboAst = {
    replaces.get(body) match {
      case Some(newAst) => newAst
      case None =>
        body match {
          case command: Command => command
          case statement: Statement =>
            statement match {
              case Block(asts, _) =>
                Block(asts.map(ast => replaceAst(ast, replaces)))
              case ITE(condition, thenAst, elseAst, _) =>
                ITE(condition, replaceAst(thenAst, replaces), replaceAst(elseAst, replaces))
              case Loop(condition, loopBody, _) =>
                Loop(condition, replaceAst(loopBody, replaces))
              case _ => throw new Exception
            }
          case _ => throw new Exception
        }
    }
  }

  def immediateParentStatements(currentNode: BrboAst, parent: Option[Statement] = None): Map[BrboAst, Statement] = {
    val currentMap: Map[BrboAst, Statement] = parent match {
      case Some(value) => Map(currentNode -> value)
      case None => Map()
    }
    val map: Map[BrboAst, Statement] = currentNode match {
      case _: Command => Map()
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            asts.flatMap(ast => immediateParentStatements(ast, Some(statement))).toMap
          case ITE(condition, thenAst, elseAst, _) =>
            immediateParentStatements(condition, Some(statement)) ++
              immediateParentStatements(thenAst, Some(statement)) ++
              immediateParentStatements(elseAst, Some(statement))
          case Loop(condition, loopBody, _) =>
            immediateParentStatements(condition, Some(statement)) ++
              immediateParentStatements(loopBody, Some(statement))
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
    currentMap ++ map
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

  @tailrec
  def immediateOuterLoop(ast: BrboAst, parentStatements: Map[BrboAst, Statement]): Option[Loop] = {
    parentStatements.get(ast) match {
      case Some(parentStatement) =>
        parentStatement match {
          case _: Block | _: ITE => immediateOuterLoop(parentStatement, parentStatements)
          case loop: Loop => Some(loop)
          case _ => throw new Exception
        }
      case None => None
    }
  }

  // The ast to be executed immediately after the given ast
  @tailrec
  def nextAst(ast: BrboAst, parentStatements: Map[BrboAst, Statement]): Option[BrboAst] = {
    parentStatements.get(ast) match {
      case Some(parentStatement) =>
        parentStatement match {
          case Block(statements, _) =>
            val index = statements.indexOf(ast)
            if (index < statements.length - 1) Some(statements(index + 1))
            else nextAst(parentStatement, parentStatements)
          case ITE(condition, thenAst, elseAst, _) =>
            if (ast == condition) {
              throw new Exception // May go to either branches
            }
            else if (ast == thenAst) {
              nextAst(parentStatement, parentStatements)
            }
            else if (ast == elseAst) {
              nextAst(parentStatement, parentStatements)
            }
            else throw new Exception
          case Loop(condition, loopBody, _) =>
            if (ast == condition) {
              throw new Exception // May either exit the loop or stay inside the loop
            }
            else if (ast == loopBody) {
              Some(condition) // Must evaluate the condition
            }
            else throw new Exception
          case _ => throw new Exception
        }
      case None => None
    }
  }

  // Insert a reset place holder at the beginning of every block
  def insertResetPlaceHolder(ast: BrboAst): BrboAst = {
    ast match {
      case command: Command => command
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            val newAsts = asts.map(ast => insertResetPlaceHolder(ast))
            Block(ResetPlaceHolder() :: newAsts)
          case ITE(condition, thenAst, elseAst, _) =>
            ITE(condition, insertResetPlaceHolder(thenAst), insertResetPlaceHolder(elseAst))
          case Loop(condition, loopBody, _) =>
            Loop(condition, insertResetPlaceHolder(loopBody))
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}
