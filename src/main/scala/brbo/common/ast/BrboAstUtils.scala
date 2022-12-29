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

  /**
   *
   * @param body
   * @param replacements
   * @param omitResetPlaceHolders Whether to replace reset placeholders with comments
   * @return
   */
  def replaceCommands(body: BrboAst, replacements: Map[Command, BrboAst], omitResetPlaceHolders: Boolean): BrboAst = {
    body match {
      case command: Command =>
        // println(s"${command.printToIR()} -> ${replacements.get(command)}")
        replacements.get(command) match {
          case Some(newAst) => newAst
          case None =>
            if (command.isInstanceOf[ResetPlaceHolder] && omitResetPlaceHolders) Comment(command.printToIR())
            else command
        }
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            Block(asts.map(ast => replaceCommands(ast, replacements, omitResetPlaceHolders)))
          case ITE(condition, thenAst, elseAst, _) =>
            ITE(
              condition,
              replaceCommands(thenAst, replacements, omitResetPlaceHolders),
              replaceCommands(elseAst, replacements, omitResetPlaceHolders)
            )
          case Loop(condition, loopBody, _) =>
            Loop(condition, replaceCommands(loopBody, replacements, omitResetPlaceHolders))
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

  // Insert reset place holders
  def insertResetPlaceHolder(ast: BrboAst): BrboAst = {
    insertResetPlaceHolder(ast, numberOfInserted = 0)._1
  }

  private def insertResetPlaceHolder(ast: BrboAst, numberOfInserted: Int): (BrboAst, Int) = {
    ast match {
      case command: Command => (command, numberOfInserted)
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            val (newAsts, newNumberOfInserted) = asts.foldLeft((Nil: List[BrboAst], numberOfInserted))({
              case ((asts, numberOfInserted), ast) =>
                val (newAst, newNumberOfInserted) = insertResetPlaceHolder(ast, numberOfInserted = numberOfInserted)
                (newAst :: asts, newNumberOfInserted)
            })
            // Not inserting a reset placeholder, because if a program has no loops,
            // then we should not bother finding a decomposition
            (Block(newAsts.reverse), newNumberOfInserted)
          case ITE(condition, thenAst, elseAst, _) =>
            val (newThenAst, newNumberOfInserted) = insertResetPlaceHolder(thenAst, numberOfInserted = numberOfInserted)
            val (newElseAst, newNumberOfInserted2) = insertResetPlaceHolder(elseAst, numberOfInserted = newNumberOfInserted)
            (ITE(condition, newThenAst, newElseAst), newNumberOfInserted2)
          case Loop(condition, loopBody, _) =>
            val resetPlaceHolder = ResetPlaceHolder(numberOfInserted + 1)
            val (newLoopBody, newNumberOfInserted) = insertResetPlaceHolder(loopBody, numberOfInserted = numberOfInserted + 1)
            // Insert at the end of the loop body
            val block = Block(List(newLoopBody, resetPlaceHolder))
            val loop = Loop(condition, block)
            // TODO: Right before the loop head?
            (loop, newNumberOfInserted)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  def getLoopConditionals(ast: BrboAst): Set[BrboExpr] = {
    ast match {
      case _: Command => Set()
      case statement: Statement =>
        statement match {
          case Block(asts, _) => asts.flatMap(ast => getLoopConditionals(ast)).toSet
          case ITE(_, thenAst, elseAst, _) =>
            getLoopConditionals(thenAst) ++ getLoopConditionals(elseAst)
          case Loop(condition, loopBody, _) =>
            getLoopConditionals(loopBody) + condition
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  abstract class InsertOperation

  object Prepend extends InsertOperation

  object Append extends InsertOperation

  def insert(ast: BrboAst, toInsert: Iterable[BrboAst], operation: InsertOperation): BrboAst = {
    val list = toInsert.toList
    ast match {
      case _: Command =>
        val block = operation match {
          case Append => ast :: list
          case Prepend => list :+ ast
          case _ => throw new Exception
        }
        Block(block)
      case statement: Statement =>
        statement match {
          case Block(asts, _) =>
            val block = operation match {
              case Append => asts ::: list
              case Prepend => list ::: asts
              case _ => throw new Exception
            }
            Block(block)
          case _: ITE | _: Loop =>
          val block = operation match {
            case Append => statement :: list
            case Prepend => list :+ statement
            case _ => throw new Exception
          }
            Block(block)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}
