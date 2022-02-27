package brbo.common.ast

import brbo.backend.verifier.modelchecker.AbstractMachine._
import brbo.backend.verifier.modelchecker.Apron
import brbo.backend.verifier.modelchecker.Apron._
import brbo.common.{BrboType, MyLogger}

object BrboExprUtils {
  private val logger = MyLogger.createLogger(BrboExprUtils.getClass, debugMode = false)

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

  /**
   *
   * @param expr      The expression to be translated to an Apron-compatible representation
   * @param valuation The valuation under which the translation happens
   * @param scope     The scope under which temporary variables (that are generated during the translation) are created
   * @return
   */
  def toApron(expr: BrboExpr, valuation: Valuation, scope: Option[Statement]): (ApronRepr, Valuation) = {
    // Integer-typed variables that are created when translating the expression into an Apron-compatible representation
    // Such variable must satisfy the associated constraints
    var temporaryVariables = Map[String, ApronVariable]()
    val existingNames = valuation.allVariables.map(v => v.identifier.name)

    def createNewVariable(): (Identifier, Int) = {
      val index = temporaryVariables.size + valuation.allVariables.size
      val name = s"v!$index"
      assert(!existingNames.contains(name))
      val temporaryVariable = Identifier(name, BrboType.INT)
      // Register the temporary variable s.t. its index can be found when recursively translating expressions
      temporaryVariables = temporaryVariables.updated(name, ApronVariable(index, None))
      (temporaryVariable, index)
    }

    def toApronHelper(expr: BrboExpr): ApronRepr = {
      val result: ApronRepr = expr match {
        case i@Identifier(identifier, typ, _) =>
          val index = {
            val index = valuation.variables.indexWhere(v => v.identifier.sameAs(i))
            if (index != -1) index
            else {
              temporaryVariables.get(identifier) match {
                case Some(ApronVariable(index, _)) => index
                case None => throw new Exception
              }
            }
          }
          val variable = Apron.mkVar(index)
          typ match {
            case brbo.common.BrboType.INT | brbo.common.BrboType.FLOAT => ApronExpr(variable)
            case brbo.common.BrboType.BOOL =>
              // Bool-typed variable v is translated into constraint v!=0, because we assume v=true iff. v!=0
              Singleton(Apron.mkNeZero(variable))
            case _ => throw new Exception
          }
        case Bool(b, _) => Singleton(Apron.mkBoolVal(b))
        case Number(n, _) => ApronExpr(Apron.mkIntVal(n))
        case StringLiteral(_, _) => throw new Exception
        case Addition(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => ApronExpr(Apron.mkAdd(left, right))
            case _ => throw new Exception
          }
        case Subtraction(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => ApronExpr(Apron.mkSub(left, right))
            case _ => throw new Exception
          }
        case Multiplication(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => ApronExpr(Apron.mkMul(left, right))
            case _ => throw new Exception
          }
        case Division(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => ApronExpr(Apron.mkDiv(left, right))
            case _ => throw new Exception
          }
        case Negation(expression, _) =>
          toApronHelper(expression) match {
            case ApronExpr(_) => throw new Exception
            case constraint: Constraint => constraint.negate()
          }
        case LessThan(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => Singleton(Apron.mkGtZero(mkSub(right, left)))
            case _ => throw new Exception
          }
        case LessThanOrEqualTo(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => Singleton(Apron.mkGeZero(mkSub(right, left)))
            case _ => throw new Exception
          }
        case GreaterThan(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => Singleton(Apron.mkGtZero(mkSub(left, right)))
            case _ => throw new Exception
          }
        case GreaterThanOrEqualTo(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => Singleton(Apron.mkGeZero(mkSub(left, right)))
            case _ => throw new Exception
          }
        case Equal(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => Singleton(Apron.mkEqZero(mkSub(left, right)))
            case _ => throw new Exception
          }
        case NotEqual(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (ApronExpr(left), ApronExpr(right)) => Singleton(Apron.mkNeZero(mkSub(left, right)))
            case _ => throw new Exception
          }
        case And(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (left: Constraint, right: Constraint) => Conjunction(left, right)
            case _ => throw new Exception
          }
        case Or(left, right, _) =>
          (toApronHelper(left), toApronHelper(right)) match {
            case (left: Constraint, right: Constraint) => Disjunction(left, right)
            case _ => throw new Exception
          }
        case FunctionCallExpr(identifier, arguments, _, _) =>
          val (temporaryVariable, index) = createNewVariable()
          identifier match {
            case PreDefinedFunctions.NDINT =>
            case PreDefinedFunctions.NDINT2 =>
              assert(arguments.size == 2)
              val lower = arguments.head
              val upper = arguments.tail.head
              toApronHelper(And(
                LessThanOrEqualTo(lower, temporaryVariable),
                LessThanOrEqualTo(temporaryVariable, upper))) match {
                case constraint: Constraint =>
                  temporaryVariables =
                    temporaryVariables.updated(temporaryVariable.name, ApronVariable(index, Some(constraint)))
                case _ => throw new Exception
              }
            case PreDefinedFunctions.NDBOOL =>
              toApronHelper(Or(
                Equal(temporaryVariable, Number(BOOLEAN_NEGATIVE)),
                Equal(temporaryVariable, Number(BOOLEAN_POSITIVE)))) match {
                case constraint: Constraint =>
                  temporaryVariables =
                    temporaryVariables.updated(temporaryVariable.name, ApronVariable(index, Some(constraint)))
                case _ => throw new Exception
              }
            case _ => throw new Exception(s"Unknown function `$identifier`")
          }
          ApronExpr(Apron.mkVar(index))
        case ITEExpr(condition, thenExpr, elseExpr, _) =>
          (thenExpr.typ, elseExpr.typ) match {
            case (BrboType.BOOL, BrboType.BOOL) =>
              toApronHelper(And(Imply(condition, thenExpr), Imply(Negation(condition), elseExpr)))
            case (BrboType.INT, BrboType.INT) =>
              val (temporaryVariable, index) = createNewVariable()
              temporaryVariables =
                temporaryVariables.updated(temporaryVariable.name, ApronVariable(index, None))
              val constraint = {
                val trueCase = toApronHelper(Imply(condition, Equal(temporaryVariable, thenExpr)))
                val falseCase = toApronHelper(Imply(Negation(condition), Equal(temporaryVariable, elseExpr)))
                (trueCase, falseCase) match {
                  case (left: Constraint, right: Constraint) => Conjunction(left, right)
                  case _ => throw new Exception
                }
              }
              temporaryVariables =
                temporaryVariables.updated(temporaryVariable.name, ApronVariable(index, Some(constraint)))
              ApronExpr(Apron.mkVar(index))
            case _ => throw new Exception
          }
        case Imply(left, right, _) => toApronHelper(Or(Negation(left), right))
      }
      result match {
        case ApronExpr(_) => assert(expr.typ == BrboType.INT)
        case _: Constraint => assert(expr.typ == BrboType.BOOL)
        case _ => throw new Exception
      }
      result
    }

    // Kick off the translation
    val apronRepr = toApronHelper(expr)
    val newValuation = temporaryVariables.foldLeft(valuation)({
      case (acc, (name, ApronVariable(_, constraint))) =>
        // Register the new variable
        val valuationWithNewVariable = acc.createUninitializedVariable(Variable(Identifier(name, BrboType.INT), scope))
        // Impose constraints on the new variable
        constraint match {
          case Some(constraint) => valuationWithNewVariable.imposeConstraint(constraint)
          case None => valuationWithNewVariable
        }
    })

    (apronRepr, newValuation)
  }
}
