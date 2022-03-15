package brbo.common.ast

import java.util.UUID

trait CommandOrExpr extends BrboAstNode with PrettyPrintToCFG

object CommandOrExpr {
  def extractUUID(commandOrExpr: CommandOrExpr): UUID = {
    commandOrExpr match {
      case expr: BrboExpr => BrboExprUtils.extractUUID(expr)
      case command: Command => BrboAstUtils.extractUUID(command)
      case _ => throw new Exception
    }
  }
}
