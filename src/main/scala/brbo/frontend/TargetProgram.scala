package brbo.frontend

import brbo.common.GhostVariableUtils.isGhostVariable
import brbo.common.ast.BrboExprUtils.{greaterThan, greaterThanOrEqualTo, lessThanOrEqualTo, notEqual}
import brbo.common.ast._
import brbo.common.{GhostVariableTyp, GhostVariableUtils, MyLogger, PreDefinedFunctions}
import brbo.frontend.JavaTreeUtils.isCommand
import brbo.frontend.TargetProgram.toBrboFunction
import com.sun.source.tree.Tree.Kind
import com.sun.source.tree._
import com.sun.source.util.TreePath

import scala.collection.JavaConverters._

case class TargetProgram(fullQualifiedClassName: String,
                         allMethods: Set[TargetMethod],
                         mainMethod: TargetMethod,
                         getLineNumber: Tree => Int,
                         getPath: Tree => TreePath,
                         sourceCode: String) {
  private val logger = MyLogger.createLogger(classOf[TargetProgram], debugMode = false)
  allMethods.foreach({
    m =>
      PreDefinedFunctions.functions.find(f => f.name == m.methodName) match {
        case Some(_) =>
          logger.info(s"Method ${m.methodName}'s name collides with a pre-defined function")
        case None =>
      }
  })

  val className: String = {
    // We expect input method's class name to be a fully qualified class name (e.g., `x.y.z.OutputHandler`)
    val index = fullQualifiedClassName.lastIndexOf(".")
    val newClassName = fullQualifiedClassName.substring(index + 1)
    logger.trace(s"New class name: `$newClassName`")
    newClassName
  }

  val packageName: Option[String] = {
    fullQualifiedClassName.lastIndexOf(".") match {
      case -1 => None
      case index =>
        val packageName = fullQualifiedClassName.substring(0, index)
        logger.trace(s"Package name: `$packageName`")
        Some(packageName)
    }
  }

  val program: BrboProgram = {
    val (mainFunction, boundAssertions) = toBrboFunction(mainMethod, allMethods)
    val otherFunctions = (allMethods - mainMethod).filter(m => m.methodName != "<init>").map(m => toBrboFunction(m, allMethods)._1).toList
    BrboProgram(s"$className.${mainMethod.methodName}", mainFunction, boundAssertions,
      PreDefinedFunctions.functionInternalRepresentations ::: otherFunctions)
  }
}

object TargetProgram {
  val MAIN_FUNCTION = "main"
  val MAX = 8
  val LARGE_INT = 10000000
  val PREDEFINED_VARIABLES: Map[String, Int] = Map("MAX" -> MAX, "LARGE_INT" -> LARGE_INT)

  private val logger = MyLogger.createLogger(TargetProgram.getClass, debugMode = false)

  def toBrboFunction(targetMethod: TargetMethod, allMethods: Set[TargetMethod]): (BrboFunction, List[BoundAssertion]) = {
    val translate = new Translate(targetMethod.variables, allMethods)
    val body = translate.toASTInternal(targetMethod.methodTree.getBody) match {
      case Left(command) => Block(List(command))
      case Right(statement) => statement
    }
    val function = BrboFunction(targetMethod.methodName, targetMethod.returnType,
      targetMethod.inputVariables.map({ case (_, identifier) => identifier }), body, translate.getResourceVariables)
    (function, translate.getBoundAssertions)
  }

  private class Translate(variables: Map[String, Identifier], allMethods: Set[TargetMethod]) {
    private var boundAssertions: List[BoundAssertion] = Nil
    private var resourceVariables: Set[Int] = Set()

    def getBoundAssertions: List[BoundAssertion] = boundAssertions

    def getResourceVariables: Set[Int] = resourceVariables

    def toAST(statementTree: StatementTree): BrboAst = {
      toASTInternal(statementTree) match {
        case Left(value) => value.asInstanceOf[BrboAst]
        case Right(value) => value.asInstanceOf[BrboAst]
      }
    }

    def toASTInternal(statementTree: StatementTree): Either[Command, Statement] = {
      if (statementTree == null) return Right(Block(Nil))

      statementTree match {
        case _ if isCommand(statementTree) =>
          statementTree match {
            case _: AssertTree => throw new Exception
            case _: EmptyStatementTree => Left(Skip())
            case _: BreakTree => Left(Break())
            case _: ContinueTree => Left(Continue())
            case tree: ExpressionStatementTree =>
              toAST(tree.getExpression) match {
                case Left(value) => Left(value)
                case Right(value) => Left(value)
              }
            case tree: ReturnTree =>
              val expression = tree.getExpression
              if (expression == null) Left(Return(None))
              else {
                toAST(expression) match {
                  case Left(brboExpr) => Left(Return(Some(brboExpr)))
                  case Right(_) => throw new Exception
                }
              }
            case tree: VariableTree =>
              variables.get(tree.getName.toString) match {
                case Some(identifier) =>
                  toAST(tree.getInitializer) match {
                    case Left(brboExpr) => Left(VariableDeclaration(identifier, brboExpr))
                    case Right(_) => throw new Exception
                  }
                case None => throw new Exception
              }

          }
        case blockTree: BlockTree =>
          Right(Block(blockTree.getStatements.asScala.map({ t => toAST(t) }).toList))
        case forLoopTree: ForLoopTree =>
          val initializers = forLoopTree.getInitializer.asScala.map({ t => toAST(t) }).toList
          val updates = forLoopTree.getUpdate.asScala.map({ t => toAST(t) }).toList
          val condition = toAST(forLoopTree.getCondition) match {
            case Left(brboExpr) => brboExpr
            case Right(_) => throw new Exception
          }
          Right(Block(initializers :+ Loop(condition, Block(toAST(forLoopTree.getStatement) :: updates))))
        case ifTree: IfTree =>
          val condition = toAST(ifTree.getCondition) match {
            case Left(brboExpr) => brboExpr
            case Right(_) => throw new Exception
          }
          val thenStatement = toAST(ifTree.getThenStatement)
          val elseStatement = toAST(ifTree.getElseStatement)
          Right(ITE(condition, thenStatement, elseStatement))
        // case labeledStatementTree: LabeledStatementTree => toAST(labeledStatementTree.getStatement)
        case whileLoopTree: WhileLoopTree =>
          val condition = toAST(whileLoopTree.getCondition) match {
            case Left(brboExpr) => brboExpr
            case Right(_) => throw new Exception
          }
          val body = toAST(whileLoopTree.getStatement)
          Right(Loop(condition, body))
        case _ => throw new Exception(s"Unsupported tree: `$statementTree`")
      }
    }

    def toAST(expressionTree: ExpressionTree): Either[BrboExpr, Command] = {
      expressionTree match {
        case _@(_: AnnotationTree | _: ArrayAccessTree |
                _: ErroneousTree | _: InstanceOfTree | _: MemberSelectTree |
                _: NewArrayTree | _: NewClassTree | _: TypeCastTree) =>
          throw new Exception(s"Unsupported expression: `$expressionTree`")
        case tree: AssignmentTree =>
          val variableName = tree.getVariable.toString
          if (isGhostVariable(variableName, GhostVariableTyp.Resource)) {
            tree.getExpression match {
              case rhs: BinaryTree =>
                if (rhs.getLeftOperand.toString == variableName) {
                  Right(Use(GhostVariableUtils.getId(variableName, GhostVariableTyp.Resource), toAST(rhs.getRightOperand).left.get))
                } else {
                  throw new Exception(s"Assignment tree `$tree` not in the form of `$variableName = $variableName + e`!")
                }
              case _ => throw new Exception
            }
          } else {
            val lhs = toAST(tree.getVariable) match {
              case Left(value) => value.asInstanceOf[Identifier]
              case Right(_) => throw new Exception
            }
            val rhs = toAST(tree.getExpression) match {
              case Left(value) => value
              case Right(_) => throw new Exception
            }
            Right(Assignment(lhs, rhs))
          }
        case tree: BinaryTree =>
          val left: BrboExpr = toAST(tree.getLeftOperand) match {
            case Left(value) => value
            case Right(_) => throw new Exception
          }
          val right: BrboExpr = toAST(tree.getRightOperand) match {
            case Left(value) => value
            case Right(_) => throw new Exception
          }

          val result = tree.getKind match {
            case Kind.PLUS => Addition(left, right)
            case Kind.MINUS => Subtraction(left, right)
            case Kind.MULTIPLY => Multiplication(left, right)
            case Kind.LESS_THAN => LessThan(left, right)
            case Kind.LESS_THAN_EQUAL => lessThanOrEqualTo(left, right)
            case Kind.GREATER_THAN => greaterThan(left, right)
            case Kind.GREATER_THAN_EQUAL => greaterThanOrEqualTo(left, right)
            case Kind.CONDITIONAL_AND => And(left, right)
            case Kind.CONDITIONAL_OR => Or(left, right)
            case Kind.EQUAL_TO => Equal(left, right)
            case Kind.NOT_EQUAL_TO => notEqual(left, right)
            case Kind.AND => And(left, right)
            case Kind.OR => Or(left, right)
            case _ => throw new Exception(s"Unsupported binary tree: `$tree`")
          }
          Left(result)
        case tree: ConditionalExpressionTree =>
          (toAST(tree.getCondition), toAST(tree.getTrueExpression), toAST(tree.getFalseExpression)) match {
            case (Left(c), Left(t), Left(f)) =>
              Left(ITEExpr(c, t, f))
            case _ => throw new Exception
          }
        case tree: CompoundAssignmentTree =>
          variables.get(tree.getVariable.toString) match {
            case Some(identifier) =>
              val update: BrboExpr = toAST(tree.getExpression) match {
                case Left(value) => value
                case Right(_) => throw new Exception
              }
              val rhs = tree.getKind match {
                case Kind.PLUS_ASSIGNMENT => Addition(identifier, update)
                case Kind.MINUS_ASSIGNMENT => Subtraction(identifier, update)
                case Kind.MULTIPLY_ASSIGNMENT => Multiplication(identifier, update)
                case _ => throw new Exception(s"Unsupported compound assignment tree `$tree`")
              }
              Right(Assignment(identifier, rhs))
            case None => throw new Exception(s"LHS is not a variable in `$tree`")
          }
        case tree: IdentifierTree =>
          val name = tree.getName.toString
          variables.get(name) match {
            case Some(identifier) => Left(identifier)
            case None =>
              PREDEFINED_VARIABLES.get(name) match {
                case Some(value) => Left(Number(value))
                case None => throw new Exception(s"Variable `$name` is neither an input or a local variable")
              }
          }
        case tree: LiteralTree =>
          tree.getKind match {
            case Kind.INT_LITERAL => Left(Number(tree.getValue.asInstanceOf[Int]))
            case Kind.BOOLEAN_LITERAL => Left(Bool(tree.getValue.asInstanceOf[Boolean]))
            case Kind.STRING_LITERAL => Left(StringLiteral(tree.getValue.asInstanceOf[String]))
            case _ => throw new Exception(s"Unsupported literal `$tree`")
          }
        case tree: MethodInvocationTree =>
          val functionName = {
            val select = tree.getMethodSelect
            assert(select.isInstanceOf[IdentifierTree])
            select.toString
          }
          val arguments = tree.getArguments.asScala.map({
            argument =>
              toAST(argument) match {
                case Left(value) => value
                case Right(_) => throw new Exception
              }
          }).toList
          functionName match {
            case PreDefinedFunctions.BoundAssertion.name =>
              boundAssertions = BoundAssertion.parse(tag = arguments.head, expr = arguments(1)) :: boundAssertions
              Right(Comment(tree.toString))
            case PreDefinedFunctions.MostPreciseBound.name | PreDefinedFunctions.LessPreciseBound.name =>
              boundAssertions = BoundAssertion.parse(tag = StringLiteral(functionName), expr = arguments.head) :: boundAssertions
              Right(Comment(tree.toString))
            case PreDefinedFunctions.UpperBound.name =>
              arguments.head match {
                case Number(groupId, _) =>
                  val resourceVariable = GhostVariableUtils.generateVariable(Some(groupId), GhostVariableTyp.Resource)
                  boundAssertions =
                    BoundAssertion.parse(tag = arguments(1), expr = lessThanOrEqualTo(resourceVariable, arguments(2))) :: boundAssertions
                  Right(Comment(tree.toString))
                case _ => throw new Exception
              }
            case PreDefinedFunctions.Use.javaFunctionName =>
              arguments.head match {
                case Number(groupId, _) =>
                  resourceVariables = resourceVariables + groupId
                  val condition = if (arguments.length == 3) arguments(2) else Bool(b = true)
                  Right(Use(Some(groupId), update = arguments(1), condition))
                case _ => throw new Exception(s"The first argument must be a number in $tree")
              }
            case PreDefinedFunctions.Reset.javaFunctionName =>
              arguments.head match {
                case Number(groupId, _) =>
                  resourceVariables = resourceVariables + groupId
                  val condition = if (arguments.length == 2) arguments(1) else Bool(b = true)
                  Right(Reset(groupId, condition))
                case _ => throw new Exception(s"The first argument must be a number in $tree")
              }
            case PreDefinedFunctions.ArrayRead.javaFunctionName =>
              Left(ArrayRead(arguments.head, arguments(1)))
            case PreDefinedFunctions.ArrayLength.javaFunctionName =>
              Left(ArrayLength(arguments.head))
            case PreDefinedFunctions.ArraySum.javaFunctionName =>
              Left(ArraySum(arguments.head))
            case _ =>
              val returnType = PreDefinedFunctions.functions.find({ f => f.javaFunctionName == functionName }) match {
                case Some(f) => f.returnType
                case None =>
                  allMethods.find(targetMethod => targetMethod.methodName == functionName) match {
                    case Some(targetMethod) => targetMethod.returnType
                    case None => throw new Exception(s"Invoking a function that is neither defined nor predefined: `$tree`")
                  }
              }
              Left(FunctionCallExpr(functionName, arguments, returnType))
          }
        case tree: ParenthesizedTree => toAST(tree.getExpression)
        case tree: UnaryTree =>
          val value: BrboExpr = toAST(tree.getExpression) match {
            case Left(value) => value
            case Right(_) => throw new Exception
          }
          tree.getKind match {
            case Kind.UNARY_MINUS => Left(Subtraction(Number(0), value))
            case Kind.UNARY_PLUS => toAST(tree.getExpression)
            case Kind.LOGICAL_COMPLEMENT => Left(Negation(value))
            case Kind.POSTFIX_INCREMENT | Kind.PREFIX_INCREMENT | Kind.POSTFIX_DECREMENT | Kind.PREFIX_DECREMENT =>
              val identifier = value.asInstanceOf[Identifier]
              val rhs = tree.getKind match {
                case Kind.POSTFIX_INCREMENT => Addition(identifier, Number(1))
                case Kind.PREFIX_INCREMENT => Addition(identifier, Number(1))
                case Kind.POSTFIX_DECREMENT => Subtraction(identifier, Number(1))
                case Kind.PREFIX_DECREMENT => Subtraction(identifier, Number(1))
                case _ => throw new Exception
              }
              // TODO: Not allow pre- / post-increments / decrements to be used as expressions
              Right(Assignment(identifier, rhs))
            case _ => throw new Exception(s"Unsupported unary tree: `$expressionTree` (Kind: `${tree.getKind}`)")
          }
      }
    }
  }
}