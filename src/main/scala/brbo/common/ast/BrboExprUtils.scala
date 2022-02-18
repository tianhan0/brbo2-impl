package brbo.common.ast

object BrboExprUtils {
  def visit(expr: BrboExpr): Unit = {
    expr match {
      case Identifier(identifier, typ, _) =>
      case Bool(b, _) =>
      case Number(n, _) =>
      case StringLiteral(value, _) =>
      case Addition(left, right, _) =>
      case Subtraction(left, right, _) =>
      case Multiplication(left, right, _) =>
      case Division(left, right, _) =>
      case Negative(expression, _) =>
      case LessThan(left, right, _) =>
      case LessThanOrEqualTo(left, right, _) =>
      case GreaterThan(left, right, _) =>
      case GreaterThanOrEqualTo(left, right, _) =>
      case Equal(left, right, _) =>
      case NotEqual(left, right, _) =>
      case And(left, right, _) =>
      case Or(left, right, _) =>
      case FunctionCallExpr(identifier, arguments, returnType, _) =>
      case ITEExpr(condition, thenExpr, elseExpr, _) =>
      case Imply(left, right, _) =>
    }
  }

  def replaceCLiteral(body: BrboExpr, from: BrboExpr, to: BrboExpr): BrboExpr = {
    if (from.prettyPrintToC() == body.prettyPrintToC()) {
      if (from.typ == body.typ) return to
      else throw new Exception("Type mismatch")
    }
    body match {
      case _@(Identifier(_, _, _) | Bool(_, _) | Number(_, _) | StringLiteral(_, _)) => body
      case Addition(left, right, _) =>
        Addition(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case Subtraction(left, right, _) =>
        Subtraction(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case Multiplication(left, right, _) =>
        Multiplication(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case Division(left, right, _) =>
        Division(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case Negative(expression, _) =>
        Negative(replaceCLiteral(expression, from, to))
      case LessThan(left, right, _) =>
        LessThan(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case LessThanOrEqualTo(left, right, _) =>
        LessThanOrEqualTo(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case GreaterThan(left, right, _) =>
        GreaterThan(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case GreaterThanOrEqualTo(left, right, _) =>
        GreaterThanOrEqualTo(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case Equal(left, right, _) =>
        Equal(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case NotEqual(left, right, _) =>
        NotEqual(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case And(left, right, _) =>
        And(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case Or(left, right, _) =>
        Or(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
      case FunctionCallExpr(identifier, arguments, returnType, _) =>
        FunctionCallExpr(identifier, arguments.map(a => replaceCLiteral(a, from, to)), returnType)
      case ITEExpr(condition, thenExpr, elseExpr, _) =>
        ITEExpr(replaceCLiteral(condition, from, to), replaceCLiteral(thenExpr, from, to), replaceCLiteral(elseExpr, from, to))
      case Imply(left, right, _) =>
        Imply(replaceCLiteral(left, from, to), replaceCLiteral(right, from, to))
    }
  }

  def collectIdentifiers(expr: BrboExpr): Set[Identifier] = {
    expr match {
      case Identifier(_, _, _) => Set(expr.asInstanceOf[Identifier])
      case _@(Bool(_, _) | Number(_, _) | StringLiteral(_, _)) => Set()
      case Addition(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case Subtraction(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case Multiplication(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case Division(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case Negative(expression, _) =>
        collectIdentifiers(expression)
      case LessThan(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case LessThanOrEqualTo(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case GreaterThan(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case GreaterThanOrEqualTo(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case Equal(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case NotEqual(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case And(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case Or(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
      case FunctionCallExpr(_, arguments, _, _) =>
        arguments.flatMap(a => collectIdentifiers(a)).toSet
      case ITEExpr(condition, thenExpr, elseExpr, _) =>
        collectIdentifiers(condition) ++ collectIdentifiers(thenExpr) ++ collectIdentifiers(elseExpr)
      case Imply(left, right, _) =>
        collectIdentifiers(left) ++ collectIdentifiers(right)
    }
  }
}
