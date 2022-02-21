package brbo.common.ast

import apron._
import brbo.backend.verifier.modelchecker.AbstractMachine.Variable
import brbo.backend.verifier.modelchecker.Apron

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

  /*def toApron(expr: BrboExpr, variables: List[ApronVariable]): Either[Texpr0Node, Conjunction] = {
    expr match {
      case Identifier(identifier, typ, _) =>
        val index = variables.indexWhere(v => v.name == identifier && v.typ == typ)
        if (index == -1) throw new Exception
        typ match {
          case brbo.common.BrboType.INT => Left(new Texpr0DimNode(index))
          case brbo.common.BrboType.BOOL => Left(new Texpr0DimNode(index))
          case _ => throw new Exception
        }
      case Bool(b, _) => Left(Apron.createBoolLiteral(b))
      case Number(n, _) => Left(Apron.createIntegerLiteral(n))
      case StringLiteral(_, _) => throw new Exception
      case Addition(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(new Texpr0BinNode(Texpr0BinNode.OP_ADD, left, right))
          case _ => throw new Exception
        }
      case Subtraction(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(new Texpr0BinNode(Texpr0BinNode.OP_SUB, left, right))
          case _ => throw new Exception
        }
      case Multiplication(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(new Texpr0BinNode(Texpr0BinNode.OP_MUL, left, right))
          case _ => throw new Exception
        }
      case Division(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) => Left(new Texpr0BinNode(Texpr0BinNode.OP_DIV, left, right))
          case _ => throw new Exception
        }
      case Negative(expression, _) =>
      case LessThan(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) =>
            Right(Conjunction(Set(Apron.generateConstraintGt(new Texpr0BinNode(Texpr0BinNode.OP_SUB, right, left)))))
          case _ => throw new Exception
        }
      case LessThanOrEqualTo(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) =>
            Right(Conjunction(Set(Apron.generateConstraintGe(new Texpr0BinNode(Texpr0BinNode.OP_SUB, right, left)))))
          case _ => throw new Exception
        }
      case GreaterThan(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) =>
            Right(Conjunction(Set(Apron.generateConstraintGt(new Texpr0BinNode(Texpr0BinNode.OP_SUB, left, right)))))
          case _ => throw new Exception
        }
      case GreaterThanOrEqualTo(left, right, _) =>
        (toApron(left, variables), toApron(right, variables)) match {
          case (Left(left), Left(right)) =>
            Right(Conjunction(Set(Apron.generateConstraintGe(new Texpr0BinNode(Texpr0BinNode.OP_SUB, left, right)))))
          case _ => throw new Exception
        }
      case Equal(left, right, _) =>
      case NotEqual(left, right, _) =>
      case And(left, right, _) =>
      case Or(left, right, _) =>
      case FunctionCallExpr(_, _, _, _) => throw new Exception
      case ITEExpr(condition, thenExpr, elseExpr, _) => // toApron(And(Imply(condition, th)))
      case Imply(left, right, _) => toApron(Or(Negative(left), right), variables)
    }
  }

  case class Conjunction(set: Set[Tcons0]) {
    def negate(): Set[Conjunction] = {
      set.map({
        constraint =>
          val newConstraint: Tcons0 =
            constraint.kind match {
              case Tcons0.EQ =>
                new Tcons0(Tcons0.DISEQ, new Texpr0Intern(constraint.toTexpr0Node))
              case Tcons0.SUPEQ =>
              case Tcons0.SUP =>
              case Tcons0.EQMOD => throw new Exception
              case Tcons0.DISEQ =>
                new Tcons0(Tcons0.EQ, new Texpr0Intern(constraint.toTexpr0Node))
            }
          Disjunction(Set(newConstraint))
      })
    }
  }

  case class Disjunction(set: Set[Tcons0])*/
}
