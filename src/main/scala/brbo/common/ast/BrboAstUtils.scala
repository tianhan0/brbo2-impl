package brbo.common.ast

object BrboAstUtils {
  def collectCommands(brboAst: BrboAst): Set[Command] = {
    brboAst match {
      case command: Command => Set(command)
      case statement: Statement =>
        statement match {
          case Block(asts, _) => asts.flatMap(ast => collectCommands(ast)).toSet
          case ITE(_, thenAst, elseAst, _) => collectCommands(thenAst) ++ collectCommands(elseAst)
          case Loop(_, body, _) => collectCommands(body)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}
