package brbo.frontend

import brbo.common.TypeUtils.BrboType.{BOOL, BrboType, INT, VOID}
import brbo.common.ast._
import brbo.common.{MyLogger, TypeUtils}
import brbo.frontend.TreeUtils.isCommand
import com.sun.source.tree.Tree.Kind
import com.sun.source.tree._
import com.sun.source.util.TreePath
import org.checkerframework.dataflow.cfg.ControlFlowGraph

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap

/**
 *
 * @param fullQualifiedClassName The class name of the method
 * @param methodTree             The method that we wish to analyze
 * @param getLineNumber          A function to get line numbers
 * @param cfg                    The control flow graph of the method
 */
case class TargetMethod(fullQualifiedClassName: String,
                        methodTree: MethodTree,
                        cfg: ControlFlowGraph,
                        getLineNumber: Tree => Int,
                        getPath: Tree => TreePath,
                        sourceCode: String) {
  private val logger = MyLogger.createLogger(classOf[TargetMethod], debugMode = false)

  TreeUtils.acceptableTree(methodTree.getBody)

  val returnType: BrboType = methodTree.getReturnType.toString match {
    case "int" => INT
    case "boolean" => BOOL
    case "void" => VOID
    case _ => throw new Exception(s"Unexpected return type: ${methodTree.getReturnType} (Kind: ${methodTree.getReturnType.getKind})")
  }

  val inputVariables: Map[String, Identifier] = TreeUtils.getAllInputVariables(methodTree).map({ case (name, typ) => (name, Identifier(name, typ)) })
  logger.trace(s"[Method `${methodTree.getName}`] Input variables: `$inputVariables`")

  private val allCommands: Set[StatementTree] = TreeUtils.collectCommands(methodTree.getBody)

  val localVariables: Map[String, Identifier] =
    if (methodTree.getBody == null) Map()
    else {
      allCommands.foldLeft(HashMap[String, BrboType]())({
        (acc, statement) =>
          statement match {
            case tree: VariableTree =>
              val name = tree.getName.toString
              val typ = org.checkerframework.javacutil.TreeUtils.typeOf(tree.getType)
              // Make sure that all variables in declared in a method have different names
              assert(!acc.keySet.contains(name), "Duplicate variable name: " + name)
              acc + (name -> TypeUtils.typeTranslation(typ))
            case _ => acc
          }
      }).map({ case (name, typ) => (name, Identifier(name, typ)) })
    }
  logger.trace(s"[Method `${methodTree.getName}`] Local variables: `$localVariables`")

  private val allVariables = inputVariables ++ localVariables
  private var mostPreciseAssertion: Option[BrboExpr] = None
  private var lessPreciseAssertion: Option[BrboExpr] = None

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

  val program: BrboProgram = toBrboProgram

  private def toBrboProgram: BrboProgram = {
    val body = toAST(methodTree.getBody) match {
      case Left(command) => Block(List(command))
      case Right(statement) => statement
    }
    val mainFunction = BrboFunction(methodTree.getName.toString, returnType, inputVariables.values.toList, body)
    BrboProgram(s"$fullQualifiedClassName${methodTree.getName}", mainFunction, mostPreciseAssertion, lessPreciseAssertion, PreDefinedBrboFunctions.allFunctionsList)
  }

  private def toASTFlatten(statementTree: StatementTree): BrboAst = {
    toAST(statementTree) match {
      case Left(value) => value.asInstanceOf[BrboAst]
      case Right(value) => value.asInstanceOf[BrboAst]
    }
  }

  private def toAST(statementTree: StatementTree): Either[Command, Statement] = {
    if (statementTree == null) Block(Nil)

    statementTree match {
      case _ if isCommand(statementTree) =>
        statementTree match {
          case _: AssertTree => throw new Exception
          case _: EmptyStatementTree => Left(Skip())
          case _: BreakTree => Left(Break())
          case _: ContinueTree => Left(Continue())
          case tree: ExpressionStatementTree =>
            toAST(tree.getExpression) match {
              case Left(value) =>
                value match {
                  case x@FunctionCallExpr(_, _, _, _) => Left(FunctionCall(x))
                  case _ => throw new Exception
                }
              case Right(value) =>
                value match {
                  case command: Command => Left(command)
                  case _ => throw new Exception
                }
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
            allVariables.get(tree.getName.toString) match {
              case Some(identifier) =>
                toAST(tree.getInitializer) match {
                  case Left(brboExpr) => Left(VariableDeclaration(identifier, brboExpr))
                  case Right(_) => throw new Exception
                }
              case None => throw new Exception
            }

        }
      case blockTree: BlockTree =>
        Right(Block(blockTree.getStatements.asScala.map({ t => toASTFlatten(t) }).toList))
      case forLoopTree: ForLoopTree =>
        val initializers = forLoopTree.getInitializer.asScala.map({ t => toASTFlatten(t) }).toList
        val updates = forLoopTree.getUpdate.asScala.map({ t => toASTFlatten(t) }).toList
        val condition = toAST(forLoopTree.getCondition) match {
          case Left(brboExpr) => brboExpr
          case Right(_) => throw new Exception
        }
        Right(Block(initializers :+ Loop(condition, Block(toASTFlatten(forLoopTree.getStatement) :: updates))))
      case ifTree: IfTree =>
        val condition = toAST(ifTree.getCondition) match {
          case Left(brboExpr) => brboExpr
          case Right(_) => throw new Exception
        }
        val thenStatement = toASTFlatten(ifTree.getThenStatement)
        val elseStatement = toASTFlatten(ifTree.getElseStatement)
        Right(ITE(condition, thenStatement, elseStatement))
      case labeledStatementTree: LabeledStatementTree => toAST(labeledStatementTree.getStatement)
      case whileLoopTree: WhileLoopTree =>
        val condition = toAST(whileLoopTree.getCondition) match {
          case Left(brboExpr) => brboExpr
          case Right(_) => throw new Exception
        }
        val body = toASTFlatten(whileLoopTree.getStatement)
        Right(Loop(condition, body))
      case _ => throw new Exception(s"Unsupported tree: `$statementTree`")
    }
  }

  private def toAST(expressionTree: ExpressionTree): Either[BrboExpr, Command] = {
    expressionTree match {
      case _@(_: AnnotationTree | _: ArrayAccessTree |
              _: ErroneousTree | _: InstanceOfTree | _: MemberSelectTree |
              _: NewArrayTree | _: NewClassTree | _: TypeCastTree) =>
        throw new Exception(s"Unsupported expression: `$expressionTree`")
      case tree: AssignmentTree =>
        val lhs = toAST(tree.getVariable) match {
          case Left(value) => value.asInstanceOf[Identifier]
          case Right(_) => throw new Exception
        }
        val rhs = toAST(tree.getExpression) match {
          case Left(value) => value
          case Right(_) => throw new Exception
        }
        Right(Assignment(lhs, rhs))
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
          case Kind.LESS_THAN_EQUAL => LessThanOrEqualTo(left, right)
          case Kind.GREATER_THAN => GreaterThan(left, right)
          case Kind.GREATER_THAN_EQUAL => GreaterThanOrEqualTo(left, right)
          case Kind.CONDITIONAL_AND => And(left, right)
          case Kind.CONDITIONAL_OR => Or(left, right)
          case Kind.EQUAL_TO => Equal(left, right)
          case Kind.NOT_EQUAL_TO => NotEqual(left, right)
          case _ => throw new Exception(s"Unsupported binary tree: `$tree`")
        }
        Left(result)
      case tree: ConditionalExpressionTree => throw new Exception(s"Not support conditional expression `$tree`")
      case tree: CompoundAssignmentTree =>
        allVariables.get(tree.getVariable.toString) match {
          case Some(identifier) =>
            val update: BrboExpr = toAST(tree.getExpression) match {
              case Left(value) => value
              case Right(_) => throw new Exception
            }
            val rhs = tree.getKind match {
              case Kind.PLUS_ASSIGNMENT => Addition(identifier, update)
              case Kind.MINUS_ASSIGNMENT => Subtraction(identifier, update)
              case Kind.MULTIPLY_ASSIGNMENT => Multiplication(identifier, update)
              case Kind.DIVIDE_ASSIGNMENT => Division(identifier, update)
              case Kind.AND_ASSIGNMENT => And(identifier, update)
              case Kind.OR_ASSIGNMENT => Or(identifier, update)
              case _ => throw new Exception(s"Unsupported compound assignment tree `$tree`")
            }
            Right(Assignment(identifier, rhs))
          case None => throw new Exception(s"LHS is not a variable in `$tree`")
        }
      case tree: IdentifierTree =>
        val name = tree.getName.toString
        allVariables.get(name) match {
          case Some(identifier) => Left(identifier)
          case None => throw new Exception(s"Variable `$name` is neither an input or a local variable")
        }
      case tree: LiteralTree =>
        tree.getKind match {
          case Kind.INT_LITERAL => Left(Number(tree.getValue.asInstanceOf[Int]))
          case Kind.BOOLEAN_LITERAL => Left(Bool(tree.getValue.asInstanceOf[Boolean]))
          case _ => throw new Exception(s"Unsupported literal `$tree`")
        }
      case tree: MethodInvocationTree =>
        val (functionName, returnType) = {
          val select = tree.getMethodSelect
          assert(select.isInstanceOf[IdentifierTree])
          val functionName = select.toString
          PreDefinedBrboFunctions.allFunctions.get(functionName) match {
            case Some(function) => (functionName, function.returnType)
            case None => throw new Exception(s"Invoking a non-predefined function `$tree`")
          }
        }
        val arguments = tree.getArguments.asScala.map({
          argument =>
            toAST(argument) match {
              case Left(value) => value
              case Right(_) => throw new Exception
            }
        }).toList
        functionName match {
          case PreDefinedBrboFunctions.MOST_PRECISE_BOUND =>
            assert(mostPreciseAssertion.isEmpty, s"We allow at most 1 call to function `${PreDefinedBrboFunctions.MOST_PRECISE_BOUND}`")
            mostPreciseAssertion = Some(arguments.head)
            Right(Skip())
          case PreDefinedBrboFunctions.LESS_PRECISE_BOUND =>
            assert(lessPreciseAssertion.isEmpty, s"We allow at most 1 call to function `${PreDefinedBrboFunctions.LESS_PRECISE_BOUND}`")
            lessPreciseAssertion = Some(arguments.head)
            Right(Skip())
          case _ => Left(FunctionCallExpr(functionName, arguments, returnType))
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
          case Kind.LOGICAL_COMPLEMENT => Left(Negative(value))
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