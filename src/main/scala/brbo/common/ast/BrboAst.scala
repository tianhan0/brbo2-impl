package brbo.common.ast

import brbo.common.BrboType._
import brbo.common.GhostVariableTyp._
import brbo.common.ast.BrboAstUtils.PrependOperation
import brbo.common.{BrboType, GhostVariableUtils, PreDefinedFunctions, SameAs}

import java.util.UUID

@SerialVersionUID(1L)
case class BrboProgram(className: String,
                       packageName: Option[String],
                       mainFunction: BrboFunction,
                       otherFunctions: List[BrboFunction] = Nil,
                       boundAssertions: List[BoundAssertion] = Nil,
                       uuid: UUID = UUID.randomUUID())
  extends Print with SameAs with Serializable {

  lazy val allFunctions: List[BrboFunction] = otherFunctions :+ mainFunction
  private val nonPredefinedFunctions = allFunctions.filterNot({
    function => PreDefinedFunctions.functions.exists(predefined => predefined.name == function.identifier)
  })

  override def printToC(indent: Int): String = {
    val functionsString = nonPredefinedFunctions.map(function => function.printToC(indent)).mkString("\n")
    s"${PreDefinedFunctions.ATOMIC_FUNCTIONS_C_DECLARATION}\n${PreDefinedFunctions.SYMBOLS_MACRO}\n$functionsString"
  }

  override def printToBrboJava(indent: Int): String = {
    val predefinedFunctions =
      s"""  public abstract int ${PreDefinedFunctions.NdInt.name}();
         |  public abstract int ${PreDefinedFunctions.NdInt2.name}(int lower, int upper);
         |  public abstract boolean ${PreDefinedFunctions.NdBool.name}();
         |  public abstract void ${PreDefinedFunctions.Assume.name}(boolean expression);
         |  public abstract void ${PreDefinedFunctions.MostPreciseBound.name}(boolean assertion);
         |  public abstract void ${PreDefinedFunctions.LessPreciseBound.name}(boolean assertion);
         |  public abstract void ${PreDefinedFunctions.ResetPlaceHolder.name}();
         |  public abstract int ${PreDefinedFunctions.ArrayLength.name}();
         |  public int ${PreDefinedFunctions.ArraySum.name}(int array) { return array; }""".stripMargin
    val nonPredefinedFunctions =
      this.nonPredefinedFunctions.map(function => function.printToBrboJavaWithBoundAssertions(indent, boundAssertions)).mkString("\n")
    s"""abstract class $className {
       |$nonPredefinedFunctions
       |$predefinedFunctions
       |}""".stripMargin
  }

  override def printToQFuzzJava(indent: Int): String = {
    val predefinedFunctions =
      s"""  int ${PreDefinedFunctions.ArrayRead.name}(int[] array, int index) { return array[index]; }
         |  int ${PreDefinedFunctions.ArrayLength.name}(int[] array) { return array.length; }
         |  int ${PreDefinedFunctions.ArraySum.name}(int[] array) {
         |    int sum = 0;
         |    for (int element : array) {
         |      sum += element;
         |    }
         |    return sum;
         |  }
         |  void ${PreDefinedFunctions.MostPreciseBound.name}(boolean assertion) {}
         |  void ${PreDefinedFunctions.LessPreciseBound.name}(boolean assertion) {}
         |  boolean ndBool2(int... values) {
         |    int sum = 0;
         |    for (int value : values) {
         |      sum += value;
         |    }
         |    // mod 2 results in a higher chance of producing an alternative value, when compared with mod 3
         |    return sum % 2 == 0;
         |  }
         |  int ndInt2(int lower, int upper) {
         |    if (upper < lower)
         |      System.exit(-1);
         |    return upper > lower ? lower + 1 : upper;
         |  }
         |${otherFunctions.find(f => f.identifier == PreDefinedFunctions.Use.name).get.printToQFuzzJava(indent = 0)}""".stripMargin
    val nonPredefinedFunctions =
      this.nonPredefinedFunctions.map(function => function.printToQFuzzJava(indent)).mkString("\n")
    val packageString: String = packageName match {
      case Some(value) => s"// package $value;"
      case None => ""
    }
    s"""$packageString
       |class $className {
       |$nonPredefinedFunctions
       |$predefinedFunctions
       |}""".stripMargin
  }

  def replaceMainFunction(newMainFunction: BrboFunction): BrboProgram =
    BrboProgram(className, packageName, newMainFunction, otherFunctions, boundAssertions)

  override def sameAs(other: Any): Boolean = {
    other match {
      case BrboProgram(otherName, otherPackageName, otherMainFunction, otherBoundAssertions, otherFunctions, _) =>
        if (otherBoundAssertions.length != boundAssertions.length || otherFunctions.length != otherFunctions.length)
          false
        else
          otherName == className &&
            otherPackageName == packageName &&
            otherMainFunction.sameAs(mainFunction) &&
            otherBoundAssertions.zip(boundAssertions).forall({
              case (a1, a2) => a1.sameAs(a2)
            }) && otherFunctions.zip(otherFunctions).forall({
            case (f1, f2) => f1.sameAs(f2)
          })
      case _ => false
    }
  }
}

/**
 *
 * @param identifier Function name
 * @param returnType Return type
 * @param parameters Function parameters
 * @param body       Function body without initializing ghost variables
 * @param groupIds   IDs of amortizations groups that *may* be used in this function,
 *                   so that their ghost variables will be initialized.
 *                   This set is empty iff. no amortization (i.e., selectively, worst-case, or fully-amortized reasoning)
 *                   has been applied to the function.
 * @param uuid       Unique ID
 */
@SerialVersionUID(2L)
case class BrboFunction(identifier: String,
                        returnType: BrboType.T,
                        parameters: List[Identifier],
                        body: Statement,
                        groupIds: Set[Int],
                        uuid: UUID = UUID.randomUUID())
  extends Print with SameAs with Serializable {
  val approximatedResourceUsage: BrboExpr = GhostVariableUtils.approximatedResourceUsage(groupIds)

  // Declare and initialize ghost variables in the function
  val bodyWithGhostInitialization: Statement = {
    val ghostVariableInitializations: List[Command] = groupIds.flatMap({
      groupId => GhostVariableUtils.declareVariables(groupId, legacy = false)
    }).toList.sortWith({ case (c1, c2) => c1.printToIR() < c2.printToIR() })
    body match {
      case Block(asts, _) => Block(ghostVariableInitializations ::: asts)
      case ITE(_, _, _, _) | Loop(_, _, _) => Block(ghostVariableInitializations :+ body)
      case _ => throw new Exception
    }
  }
  private lazy val legacyGhostVariables: List[Identifier] =
    groupIds.toList.sorted.map(id => GhostVariableUtils.generateVariable(Some(id), Resource, legacy = true))

  private lazy val legacyGhostVariablesSum = legacyGhostVariables.foldLeft(Number(0): BrboExpr)({
    case (soFar, variable) => Addition(soFar, variable)
  })

  lazy val nonGhostVariables: List[Identifier] = {
    val variables = BrboAstUtils.collectCommands(body).flatMap({
      case VariableDeclaration(identifier, _, _) => Some(identifier)
      case _ => None
    }).toList ::: parameters
    variables.sortWith({
      case (v1, v2) => v1.printToIR() < v2.printToIR()
    })
  }

  override def printToC(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePair(CPrintType)}").mkString(", ")
    val indentString = " " * (indent + DEFAULT_INDENT)
    s"$indentString${BrboType.PrintType.print(returnType, CPrintType)} $identifier($parametersString)\n${bodyWithGhostInitialization.printToC(indent + DEFAULT_INDENT)}"
  }

  override def printToBrboJava(indent: Int): String = printToBrboJavaWithBoundAssertions(indent, boundAssertions = Nil)

  override def printToQFuzzJava(indent: Int): String = {
    val parametersString = parameters.map(pair => s"${pair.typeNamePair(QFuzzPrintType)}").mkString(", ")
    s"${indentString(indent + DEFAULT_INDENT)}${BrboType.PrintType.print(returnType, QFuzzPrintType)} $identifier($parametersString)\n" +
      s"${body.printToQFuzzJava(indent + DEFAULT_INDENT)}"
  }

  def printToBrboJavaWithBoundAssertions(indent: Int, boundAssertions: List[BoundAssertion]): String = {
    // TODO: Assume that for any array-typed input x, there exists at most 1 arrayRead(x, i) for any i
    val arrayGhostVariables: List[Identifier] = parameters.flatMap({
      case parameter@Identifier(_, BrboType.ARRAY(BrboType.INT), _) =>
        List(
          BrboAstUtils.arrayGhostVariable(parameter, BrboAstUtils.ArrayTemporary),
          BrboAstUtils.arrayGhostVariable(parameter, BrboAstUtils.ArrayLastIndex)
        )
      case Identifier(_, BrboType.INT, _) => Nil
      case Identifier(_, BrboType.BOOL, _) => Nil
      case _ => throw new Exception
    })
    // Declare ghost variables used for translating array operations
    val arrayGhostVariableDeclarations =
      arrayGhostVariables.map({ arrayGhostVariable => VariableDeclaration(arrayGhostVariable, Number(0)) })
    val boundAssertionExprs: List[BrboExpr] = boundAssertionExpressions(boundAssertions)
    val parametersString = parameters.map(pair => s"${pair.typeNamePair(CPrintType)}").mkString(", ")
    val ghostVariableDeclarations: List[Command] = groupIds.flatMap({
      groupId => GhostVariableUtils.declareVariables(groupId, legacy = true)
    }).toList.sortWith({ case (c1, c2) => c1.printToIR() < c2.printToIR() })
    val bodyWithInitialization = BrboAstUtils.insert(
      ast = body,
      toInsert = ghostVariableDeclarations ::: arrayGhostVariableDeclarations ::: boundAssertionExprs,
      operation = PrependOperation
    )
    s"${indentString(indent + DEFAULT_INDENT)}${BrboType.PrintType.print(returnType, CPrintType)} $identifier($parametersString) \n" +
      s"${bodyWithInitialization.printToBrboJava(indent + DEFAULT_INDENT)}"
  }

  private def boundAssertionExpressions(boundAssertions: List[BoundAssertion]): List[BrboExpr] = {
    boundAssertions.map({
      boundAssertion =>
        boundAssertion.tag match {
          case PreDefinedFunctions.MostPreciseBound.name =>
            val assertion = boundAssertion.replaceResourceVariable(into = legacyGhostVariablesSum)
            FunctionCallExpr(PreDefinedFunctions.MostPreciseBound.name, List(assertion), PreDefinedFunctions.MostPreciseBound.returnType)
          case PreDefinedFunctions.LessPreciseBound.name =>
            val assertion = boundAssertion.replaceResourceVariable(into = legacyGhostVariablesSum)
            FunctionCallExpr(PreDefinedFunctions.LessPreciseBound.name, List(assertion), PreDefinedFunctions.LessPreciseBound.returnType)
          case _ => throw new Exception
        }
    })
  }

  def replaceBodyWithoutGhostInitialization(newBody: Statement): BrboFunction = BrboFunction(identifier, returnType, parameters, newBody, groupIds)

  def replaceGroupIds(newGroupIds: Set[Int]): BrboFunction = BrboFunction(identifier, returnType, parameters, body, newGroupIds)

  def sameAs(other: Any): Boolean = {
    other match {
      case BrboFunction(otherIdentifier, otherReturnType, otherParameters, otherBodyNoInitialization, otherGroupIds, _) =>
        val sameParameters = {
          if (otherParameters.length != parameters.length) false
          else otherParameters.zip(parameters).forall({ case (i1, i2) => i1.sameAs(i2) })
        }
        otherIdentifier == identifier && otherReturnType == returnType && sameParameters &&
          otherBodyNoInitialization.sameAs(body) && otherGroupIds == groupIds
      case _ => false
    }
  }
}

abstract class BrboAst extends SameAs with Serializable with Print

abstract class Statement(val uuid: UUID) extends BrboAst

abstract class Command(val uuid: UUID) extends BrboAst with GetFunctionCalls with UseDefVariables {
  def printToBrboJava(indent: Int): String = printToC(indent)

  def printToQFuzzJava(indent: Int): String = printToC(indent)

  final def printToIR(): String = {
    this match {
      case _: BrboExpr => printToCInternal(indent = 0)
      case BeforeFunctionCall(callee, actualArguments, _) =>
        val argumentsString =
          if (actualArguments.nonEmpty) s" with `${actualArguments.map(a => a.printToIR()).mkString(", ")}`"
          else " no arguments"
        s"Call function `${callee.identifier}`$argumentsString"
      case use@Use(_, update, condition, _) =>
        val conditionString = condition match {
          case Bool(true, _) => ""
          case _ => s"if (${condition.printNoOuterBrackets}) "
        }
        s"${conditionString}use ${use.resourceVariable.name} ${update.printToIR()}"
      case reset@Reset(_, condition, _) =>
        val conditionString = condition match {
          case Bool(true, _) => ""
          case _ => s"if (${condition.printNoOuterBrackets}) "
        }
        s"${conditionString}reset ${reset.resourceVariable.name}"
      case _: Command => printToC(indent = 0)
      case _ => throw new Exception()
    }
  }

  final override def printToC(indent: Int): String = {
    this match {
      case _: BrboExpr => printToCInternal(indent) + ";"
      case _: Command => printToCInternal(indent)
      case _ => throw new Exception()
    }
  }

  def printToCInternal(indent: Int): String

  override def getUses: Set[Identifier] = Set()

  override def getDefs: Set[Identifier] = Set()

  override def getFunctionCalls: List[FunctionCallExpr] = Nil
}

case class Block(asts: List[BrboAst], override val uuid: UUID = UUID.randomUUID()) extends Statement(uuid) {
  private val simplifiedAsts: List[BrboAst] = BrboAstUtils.flatten(this)

  override def printToC(indent: Int): String = {
    s"${indentString(indent)}{\n${simplifiedAsts.map(t => t.printToC(indent + DEFAULT_INDENT)).mkString("\n")}\n${indentString(indent)}}"
  }

  override def printToBrboJava(indent: Int): String = {
    s"${indentString(indent)}{\n${simplifiedAsts.map(t => t.printToBrboJava(indent + DEFAULT_INDENT)).mkString("\n")}\n${indentString(indent)}}"
  }

  override def printToQFuzzJava(indent: Int): String = {
    s"${indentString(indent)}{\n${simplifiedAsts.map(t => t.printToQFuzzJava(indent + DEFAULT_INDENT)).mkString("\n")}\n${indentString(indent)}}"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Block(otherAsts, _) =>
        if (otherAsts.length != asts.length) false
        else otherAsts.zip(asts).forall({ case (s1, s2) => s1.sameAs(s2) })
      case _ => false
    }
  }
}

case class Loop(condition: BrboExpr, loopBody: BrboAst, override val uuid: UUID = UUID.randomUUID()) extends Statement(uuid) {
  private val loopBodyBlock = loopBody match {
    case _: Command | _: Loop | _: ITE => Block(List(loopBody))
    case _: Block => loopBody
    case _ => throw new Exception
  }

  private val conditionString = condition.printNoOuterBrackets

  override def printToC(indent: Int): String = {
    val bodyString = loopBodyBlock.printToC(indent)
    s"${indentString(indent)}while ($conditionString)\n$bodyString"
  }

  override def printToBrboJava(indent: Int): String = {
    val bodyString = loopBodyBlock.printToBrboJava(indent)
    s"${indentString(indent)}while ($conditionString)\n$bodyString"
  }

  override def printToQFuzzJava(indent: Int): String = {
    val bodyString = loopBodyBlock.printToQFuzzJava(indent)
    s"${indentString(indent)}while ($conditionString)\n$bodyString"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Loop(otherCondition, otherLoopBody, _) =>
        otherCondition.sameAs(condition) && otherLoopBody.sameAs(loopBody)
      case _ => false
    }
  }
}

case class ITE(condition: BrboExpr, thenAst: BrboAst, elseAst: BrboAst, override val uuid: UUID = UUID.randomUUID()) extends Statement(uuid) {
  private val thenBlock = thenAst match {
    case _: Command | _: Loop | _: ITE => Block(List(thenAst))
    case _: Block => thenAst
    case _ => throw new Exception
  }

  private val elseBlock = elseAst match {
    case _: Command | _: Loop | _: ITE => Block(List(elseAst))
    case _: Block => elseAst
    case _ => throw new Exception
  }

  private val conditionString = condition.printNoOuterBrackets

  override def printToC(indent: Int): String = {
    val thenString = thenBlock.printToC(indent)
    val elseString = elseBlock.printToC(indent)
    s"${indentString(indent)}if ($conditionString)\n$thenString\n${indentString(indent)}else\n$elseString"
  }

  override def printToBrboJava(indent: Int): String = {
    val thenString = thenBlock.printToBrboJava(indent)
    val elseString = elseBlock.printToBrboJava(indent)
    s"${indentString(indent)}if ($conditionString)\n$thenString\n${indentString(indent)}else\n$elseString"
  }

  override def printToQFuzzJava(indent: Int): String = {
    val thenString = thenBlock.printToQFuzzJava(indent)
    val elseString = elseBlock.printToQFuzzJava(indent)
    s"${indentString(indent)}if ($conditionString)\n$thenString\n${indentString(indent)}else\n$elseString"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case ITE(otherCondition, otherThenAst, otherElseAst, _) =>
        otherCondition.sameAs(condition) && otherThenAst.sameAs(thenAst) && otherElseAst.sameAs(elseAst)
      case _ => false
    }
  }
}

case class Assume(condition: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  assert(condition.typ == BOOL)

  override def printToCInternal(indent: Int): String = {
    val conditionString = condition.printNoOuterBrackets
    s"${indentString(indent)}assume($conditionString);"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = condition.getFunctionCalls

  override def getUses: Set[Identifier] = condition.getUses

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Assume(otherCondition, _) => condition.sameAs(otherCondition)
          case _ => false
        }
      case _ => false
    }
  }
}

case class VariableDeclaration(identifier: Identifier, initialValue: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  private val initialValueString = identifier.typ match {
    case INT => assert(initialValue.typ == INT, s"variable: `$identifier` (type: `${identifier.typ}`); initialValue: `$initialValue` (type: `${initialValue.typ}`)"); initialValue.printNoOuterBrackets
    case BOOL => assert(initialValue.typ == BOOL); initialValue.printNoOuterBrackets
    case STRING => assert(initialValue.typ == STRING); initialValue.printNoOuterBrackets
    case _ => throw new Exception
  }

  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)}${BrboType.PrintType.print(identifier.typ, CPrintType)} ${identifier.name} = $initialValueString;"
  }

  override def printToQFuzzJava(indent: Int): String = {
    s"${indentString(indent)}${BrboType.PrintType.print(identifier.typ, QFuzzPrintType)} ${identifier.name} = $initialValueString;"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = initialValue.getFunctionCalls

  override def getUses: Set[Identifier] = initialValue.getUses

  override def getDefs: Set[Identifier] = Set(identifier)

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case VariableDeclaration(otherIdentifier, otherInitialValue, _) =>
            otherIdentifier.sameAs(identifier) && otherInitialValue.sameAs(initialValue)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Assignment(identifier: Identifier, expression: BrboExpr, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  assert(identifier.typ == expression.typ)

  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)}${identifier.name} = ${expression.printNoOuterBrackets};"
  }

  override def printToBrboJava(indent: Int): String = {
    expression match {
      case ArrayRead(array, _, _) =>
        // Assume that every array element is read exactly once in any run
        val temporaryVariable = BrboAstUtils.arrayGhostVariable(array.asInstanceOf[Identifier], BrboAstUtils.ArrayTemporary)
        val lastIndexVariable = BrboAstUtils.arrayGhostVariable(array.asInstanceOf[Identifier], BrboAstUtils.ArrayLastIndex)
        val arraySum = FunctionCallExpr(
          identifier = PreDefinedFunctions.ArraySum.name,
          arguments = List(identifier),
          returnType = PreDefinedFunctions.ArraySum.returnType
        )
        val assignTemporary = Assignment(
          temporaryVariable,
          FunctionCallExpr(
            identifier = PreDefinedFunctions.NdInt2.name,
            arguments = List(lastIndexVariable, arraySum),
            returnType = PreDefinedFunctions.NdInt2.returnType
          )
        )
        val updateLastIndex = Assignment(
          lastIndexVariable,
          Addition(lastIndexVariable, temporaryVariable)
        )
        val actualAssignment = Assignment(identifier, temporaryVariable)
        val block = Block(List(assignTemporary, updateLastIndex, actualAssignment))
        block.printToBrboJava(indent)
      case _ => super.printToBrboJava(indent)
    }
  }

  override def getFunctionCalls: List[FunctionCallExpr] = expression.getFunctionCalls

  override def getUses: Set[Identifier] = expression.getUses + identifier

  override def getDefs: Set[Identifier] = expression.getDefs

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Assignment(otherIdentifier, otherExpression, _) =>
            otherIdentifier.sameAs(identifier) && otherExpression.sameAs(expression)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Skip(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)};"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Skip(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class Return(expression: Option[BrboExpr], override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCInternal(indent: Int): String = {

    val valueString =
      expression match {
        case Some(value) => s" ${value.printNoOuterBrackets}"
        case None => ""
      }
    s"${indentString(indent)}return$valueString;"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = {
    expression match {
      case Some(value2) => value2.getFunctionCalls
      case None => Nil
    }
  }

  override def getUses: Set[Identifier] = {
    expression match {
      case Some(v) => v.getUses
      case None => Set()
    }
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Return(otherValue, _) =>
            (otherValue, expression) match {
              case (Some(otherValue), Some(value)) => otherValue.sameAs(value)
              case (None, None) => true
              case _ => false
            }
          case _ => false
        }
      case _ => false
    }
  }
}

case class Break(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)}break;"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Break(_) => true
      case _ => false
    }
  }
}

case class Continue(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)}continue;"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Continue(_) => true
      case _ => false
    }
  }
}

case class LabeledCommand(label: String, command: Command, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  assert(!command.isInstanceOf[LabeledCommand])
  assert(!command.isInstanceOf[GhostCommand], s"Not allow ghost commands to be labeled, due to the translation in BrboAstInC")

  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)}$label: ${command.printToC(0)}"
  }

  override def getFunctionCalls: List[FunctionCallExpr] = command.getFunctionCalls

  override def getUses: Set[Identifier] = command.getUses

  override def getDefs: Set[Identifier] = command.getDefs

  override def sameAs(other: Any): Boolean = {
    other match {
      case LabeledCommand(otherLabel, otherCommand, _) =>
        otherLabel == label && otherCommand.sameAs(command)
      case _ => false
    }
  }
}

case class Comment(content: String, override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) {
  override def printToCInternal(indent: Int): String = {
    s"${indentString(indent)}// ${content.replace("\n", " ")}"
  }

  override def sameAs(other: Any): Boolean = {
    other match {
      case Comment(otherContent, _) => content == otherContent
      case _ => false
    }
  }
}

sealed trait CFGOnly

case class FunctionExit(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCInternal(indent: Int): String = "[Function Exit]"

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case FunctionExit(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class LoopExit(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCInternal(indent: Int): String = "[Loop Exit]"

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case LoopExit(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class Empty(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCInternal(indent: Int): String = "[Empty Node]"

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Empty(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

case class BranchingHead(override val uuid: UUID = UUID.randomUUID()) extends Command(uuid) with CFGOnly {
  override def printToCInternal(indent: Int): String = "[Branch Head]"

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case BranchingHead(_) => true
          case _ => false
        }
      case _ => false
    }
  }
}

abstract class GhostCommand(uuid: UUID) extends Command(uuid) {
  def replace(newGroupId: Int): GhostCommand
}

case class Use( // When None, this command represents updating the original resource variable.
                // Otherwise, this command represents updating a resource variable for some amortization group
                groupId: Option[Int],
                update: BrboExpr,
                condition: BrboExpr = Bool(b = true),
                override val uuid: UUID = UUID.randomUUID()) extends GhostCommand(uuid) {
  assert(update.typ == BrboType.INT)
  assert(condition.typ == BrboType.BOOL)

  val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource)

  val assignmentCommand: Assignment = Assignment(resourceVariable, Addition(resourceVariable, update))

  override def getFunctionCalls: List[FunctionCallExpr] = update.getFunctionCalls

  override def printToCInternal(indent: Int): String = {
    val ast = generateAst(assignmentCommand)
    ast.printToC(indent)
  }

  override def printToBrboJava(indent: Int): String = {
    if (groupId.isEmpty) {
      // When a use command is not assigned with a group (i.e., the command is not decomposed),
      // print the original assignment commands over `R`.
      return printToCInternal(indent)
    }
    val resourceVariable: Identifier = GhostVariableUtils.generateVariable(groupId, Resource, legacy = true)
    val assignmentCommand: Assignment = Assignment(resourceVariable, Addition(resourceVariable, update))
    val ast = generateAst(assignmentCommand)
    ast.printToBrboJava(indent)
  }

  private def generateAst(assignment: Assignment): BrboAst = {
    condition match {
      case Bool(true, _) => assignment
      case _ => ITE(condition, assignment, Skip())
    }
  }

  override def replace(newGroupId: Int): Use = Use(Some(newGroupId), update, condition)

  override def getUses: Set[Identifier] = update.getUses ++ condition.getUses

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Use(otherGroupId, otherUpdate, otherCondition, _) =>
            otherGroupId == groupId && otherUpdate.sameAs(update) && otherCondition.sameAs(condition)
          case _ => false
        }
      case _ => false
    }
  }
}

case class Reset(groupId: Int, condition: BrboExpr = Bool(b = true),
                 override val uuid: UUID = UUID.randomUUID()) extends GhostCommand(uuid) {
  assert(condition.typ == BrboType.BOOL)

  val (resourceVariable: Identifier, starVariable: Identifier, counterVariable: Identifier) =
    GhostVariableUtils.generateVariables(Some(groupId), legacy = false)

  val updateStarCommand: Assignment = {
    val iteExpr = ITEExpr(LessThan(starVariable, resourceVariable), resourceVariable, starVariable)
    Assignment(starVariable, iteExpr)
  }
  val compareStarWithResource: LessThan = LessThan(starVariable, resourceVariable)
  val assignToStar: Assignment = Assignment(starVariable, resourceVariable)
  val updateStarITE: ITE = ITE(compareStarWithResource, assignToStar, Skip())
  val updateResource: Assignment = Assignment(resourceVariable, Number(0))
  val updateCounter: Assignment = Assignment(counterVariable, Addition(counterVariable, Number(1)))

  override def printToCInternal(indent: Int): String = {
    val ast = generateAst(updateStar = updateStarITE, updateResource = updateResource, updateCounter = updateCounter)
    ast.map(ast => ast.printToC(indent)).mkString("\n")
  }

  override def printToBrboJava(indent: Int): String = {
    val (resourceVariable: Identifier, starVariable: Identifier, counterVariable: Identifier) =
      GhostVariableUtils.generateVariables(Some(groupId), legacy = true)
    val maxStatement: ITE = {
      val maxComparison: LessThan = LessThan(starVariable, resourceVariable)
      val maxAssignment: Assignment = Assignment(starVariable, resourceVariable)
      ITE(maxComparison, maxAssignment, Skip())
    }
    val resetCommand: Assignment = Assignment(resourceVariable, Number(0))
    val counterCommand: Assignment = Assignment(counterVariable, Addition(counterVariable, Number(1)))
    val ast = generateAst(updateStar = maxStatement, updateResource = resetCommand, updateCounter = counterCommand)
    ast.map(ast => ast.printToBrboJava(indent)).mkString("\n")
  }

  private def generateAst(updateStar: ITE, updateResource: Command, updateCounter: Command): List[BrboAst] = {
    val list = List(updateStar, updateResource, updateCounter)
    condition match {
      case Bool(true, _) => list
      case _ =>
        val ite = ITE(condition, Block(list), Skip())
        List(ite)
    }
  }

  def replace(newGroupId: Int): Reset = Reset(newGroupId, condition)

  override def getUses: Set[Identifier] = condition.getUses

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case Reset(otherGroupId, otherCondition, _) =>
            otherGroupId == groupId && otherCondition.sameAs(condition)
          case _ => false
        }
      case _ => false
    }
  }
}

// This is a place holder that will be replaced with reset commands
case class ResetPlaceHolder(id: Int, override val uuid: UUID = UUID.randomUUID()) extends GhostCommand(uuid) {
  override def printToCInternal(indent: Int): String = s"${indentString(indent)}${PreDefinedFunctions.ResetPlaceHolder.name}_$id();"

  override def sameAs(other: Any): Boolean = other match {
    case ResetPlaceHolder(otherId, _) => id == otherId
    case _ => false
  }

  override def replace(newGroupId: Int): GhostCommand = ???
}

sealed trait CexPathOnly

case class BeforeFunctionCall(callee: BrboFunction, actualArguments: List[BrboExpr], override val uuid: UUID = UUID.randomUUID())
  extends Command(uuid) with CexPathOnly {
  override def printToCInternal(indent: Int): String = throw new Exception

  override def sameAs(other: Any): Boolean = {
    other match {
      case command: Command =>
        command match {
          case BeforeFunctionCall(otherCallee, otherActualArguments, _) =>
            val sameArguements =
              if (otherActualArguments.length != actualArguments.length) false
              else otherActualArguments.zip(actualArguments).forall({ case (a1, a2) => a1.sameAs(a2) })
            callee.sameAs(otherCallee) && sameArguements
          case _ => false
        }
      case _ => false
    }
  }
}