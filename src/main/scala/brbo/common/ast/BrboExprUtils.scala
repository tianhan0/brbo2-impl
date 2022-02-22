package brbo.common.ast

import apron._
import brbo.backend.verifier.modelchecker.Apron
import brbo.backend.verifier.modelchecker.Apron._
import brbo.common.BrboType

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
      case Negation(expression, _) =>
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
      case Negation(expression, _) =>
        Negation(replaceCLiteral(expression, from, to))
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
      case Negation(expression, _) =>
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

  def toApron(expr: BrboExpr, variables: List[Identifier]): Either[Texpr0Node, Constraint] = {
    val result: Either[Texpr0Node, Constraint] = expr match {
      case Identifier(identifier, typ, _) =>
        val index = variables.indexWhere(v => v.identifier == identifier && v.typ == typ)
        if (index == -1) throw new Exception
        val variable = Apron.mkVar(index)
        typ match {
          case brbo.common.BrboType.INT => Left(variable)
          case brbo.common.BrboType.BOOL =>
            // Bool-typed variable v is translated into constraint v!=0, because we assume v=true iff. v!=0
            Right(Singleton(Apron.mkNe(variable)))
          case _ => throw new Exception
        }
      case Bool(b, _) => Right(Singleton(Apron.mkBoolVal(b)))
      case Number(n, _) => Left(Apron.mkIntVal(n))
      case StringLiteral(_, _) => throw new Exception
      case Addition(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(Apron.mkAdd(left, right))
          case _ => throw new Exception
        }
      case Subtraction(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(Apron.mkSub(left, right))
          case _ => throw new Exception
        }
      case Multiplication(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(Apron.mkMul(left, right))
          case _ => throw new Exception
        }
      case Division(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(Apron.mkDiv(left, right))
          case _ => throw new Exception
        }
      case Negation(expression, _) =>
        toApron(expression, variables) match {
          case Left(_) => throw new Exception
          case Right(constraint) => Right(constraint.negate())
        }
      case LessThan(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Right(Singleton(Apron.mkGt(mkSub(right, left))))
          case _ => throw new Exception
        }
      case LessThanOrEqualTo(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Right(Singleton(Apron.mkGe(mkSub(right, left))))
          case _ => throw new Exception
        }
      case GreaterThan(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Right(Singleton(Apron.mkGt(mkSub(left, right))))
          case _ => throw new Exception
        }
      case GreaterThanOrEqualTo(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Right(Singleton(Apron.mkGe(mkSub(left, right))))
          case _ => throw new Exception
        }
      case Equal(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Right(Singleton(Apron.mkEq(mkSub(left, right))))
          case _ => throw new Exception
        }
      case NotEqual(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Right(Singleton(Apron.mkNe(mkSub(left, right))))
          case _ => throw new Exception
        }
      case And(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Right(left), Right(right)) => Right(Conjunction(left, right))
          case _ => throw new Exception
        }
      case Or(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Right(left), Right(right)) => Right(Disjunction(left, right))
          case _ => throw new Exception
        }
      case FunctionCallExpr(_, _, _, _) => throw new Exception
      case ITEExpr(condition, thenExpr, elseExpr, _) =>
        (thenExpr.typ, elseExpr.typ) match {
          case (BrboType.BOOL, BrboType.BOOL) =>
            toApron(And(Imply(condition, thenExpr), Imply(Negation(condition), elseExpr)), variables)
          case (BrboType.INT, BrboType.INT) => throw new Exception // TODO: Support this
          case _ => throw new Exception
        }
      case Imply(left, right, _) => toApron(Or(Negation(left), right), variables)
    }
    result match {
      case Left(_) => assert(expr.typ == BrboType.INT)
      case Right(_) => assert(expr.typ == BrboType.BOOL)
    }
    result
  }
}
