package brbo.common.ast

case class BrboProgramInC(originalProgram: BrboProgram) {
  val (program: BrboProgram, map: Map[Either[Command, BrboExpr], GhostCommand]) = programToC

  private def programToC: (BrboProgram, Map[Either[Command, BrboExpr], GhostCommand]) = {
    val (newMainFunction, mainMap) = {
      val c = BrboFunctionInC(originalProgram.mainFunction)
      (c.function, c.map)
    }
    val (functions, map) = originalProgram.functions.foldLeft(Nil: List[BrboFunction], mainMap)({
      (acc, function) =>
        val c = BrboFunctionInC(function)
        (c.function :: acc._1, acc._2 ++ c.map)
    })
    val newProgram = BrboProgram(originalProgram.name, newMainFunction, originalProgram.groupIds, originalProgram.mostPreciseAssertion, originalProgram.lessPreciseAssertion, functions.reverse)
    (newProgram, map)
  }
}

case class BrboFunctionInC(originalFunction: BrboFunction) {
  // Map commands or expressions in the C version that are translated from uses or resets in the original program
  val (function: BrboFunction, map: Map[Either[Command, BrboExpr], GhostCommand]) = functionToC

  private def functionToC: (BrboFunction, Map[Either[Command, BrboExpr], GhostCommand]) = {
    var map: Map[Either[Command, BrboExpr], GhostCommand] = Map()

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
          map = map + (Right(reset.maxComparison) -> reset)
          map = map + (Left(reset.maxAssignment) -> reset)
          map = map + (Left(reset.resetCommand) -> reset)
          map = map + (Left(reset.counterCommand) -> reset)
          map = map + (Right(condition) -> reset)
          val block = Block(List(reset.maxStatement, reset.resetCommand, reset.counterCommand))
          ITE(condition, block, Skip())
        case use@Use(_, _, condition, _) =>
          map = map + (Left(use.assignmentCommand) -> use)
          map = map + (Right(condition) -> use)
          ITE(condition, use.assignmentCommand, Skip())
        case FunctionCall(functionCallExpr, _) => FunctionCall(functionCallExpr)
        case LabeledCommand(label, command, _) => LabeledCommand(label, commandToC(command).asInstanceOf[Command])
        case CallFunction(_, _) => throw new Exception
        case Assignment(variable, expression, _) => Assignment(variable, expression)
        case VariableDeclaration(variable, initialValue, _) => VariableDeclaration(variable, initialValue)
      }
    }

    val newFunction = BrboFunction(originalFunction.identifier, originalFunction.returnType, originalFunction.parameters, astToC(originalFunction.body).asInstanceOf[Statement])
    (newFunction, map)
  }
}
