package brbo.common.ast

case class BrboCProgram(originalProgram: BrboProgram) {
  val (program: BrboProgram, map: Map[Command, GhostCommand]) = programToC

  private def programToC: (BrboProgram, Map[Command, GhostCommand]) = {
    val (newMainFunction, mainMap) = {
      val c = BrboCFunction(originalProgram.mainFunction)
      (c.function, c.map)
    }
    val (otherFunctions, map) = originalProgram.otherFunctions.foldLeft(Nil: List[BrboFunction], mainMap)({
      (acc, function) =>
        val c = BrboCFunction(function)
        (c.function :: acc._1, acc._2 ++ c.map)
    })
    val newProgram = BrboProgram(
      className = originalProgram.className,
      packageName = originalProgram.packageName,
      mainFunction = newMainFunction,
      otherFunctions = otherFunctions.reverse,
      boundAssertions = originalProgram.boundAssertions,
    )
    (newProgram, map)
  }
}

case class BrboCFunction(originalFunction: BrboFunction) {
  // Map commands or expressions in the C version that are translated from uses or resets in the original program
  val (function: BrboFunction, map: Map[Command, GhostCommand]) = functionToC

  private def functionToC: (BrboFunction, Map[Command, GhostCommand]) = {
    var map: Map[Command, GhostCommand] = Map()

    def astToC(ast: BrboAst): BrboAst = {
      ast match {
        case command: Command => commandToC(command)
        case statement2: Statement => statementToC(statement2)
        case _ => throw new Exception
      }
    }

    def statementToC(statement: Statement): BrboAst = {
      statement match {
        case Block(asts, _) =>
          Block(asts.map(ast => astToC(ast)))
        case ITE(condition, thenAst, elseAst, _) =>
          // Use the original conditional expressions, instead of creating new conditional expressions
          ITE(condition, astToC(thenAst), astToC(elseAst))
        case Loop(condition, body, _) =>
          // Use the original conditional expressions, instead of creating new conditional expressions
          Loop(condition, astToC(body))
        case _ => throw new Exception
      }
    }

    def commandToC(command: Command): BrboAst = {
      command match {
        case Skip(_) => Skip()
        case Break(_) => Break()
        case Continue(_) => Continue()
        case _: CFGOnly => throw new Exception
        case Return(value, _) => Return(value)
        case reset@Reset(_, condition, _) =>
          map = map + (reset.compareStarWithResource -> reset)
          map = map + (reset.assignToStar -> reset)
          map = map + (reset.updateResource -> reset)
          map = map + (reset.updateCounter -> reset)
          map = map + (condition -> reset)
          val block = Block(List(reset.updateStarITE, reset.updateResource, reset.updateCounter))
          ITE(condition, block, Skip())
        case use@Use(_, _, condition, _) =>
          map = map + (use.assignmentCommand -> use)
          map = map + (condition -> use)
          ITE(condition, use.assignmentCommand, Skip())
        case LabeledCommand(label, command, _) => LabeledCommand(label, commandToC(command).asInstanceOf[Command])
        case BeforeFunctionCall(_, _, _) => throw new Exception
        case Assignment(variable, expression, _) => Assignment(variable, expression)
        case VariableDeclaration(variable, initialValue, _) => VariableDeclaration(variable, initialValue)
      }
    }

    val newFunction = BrboFunction(originalFunction.identifier, originalFunction.returnType, originalFunction.parameters,
      astToC(originalFunction.body).asInstanceOf[Statement], originalFunction.groupIds, originalFunction.useResource)
    (newFunction, map)
  }
}
