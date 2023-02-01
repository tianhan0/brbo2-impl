package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.ast.BrboAstUtils.{AppendOperation, PrependOperation}
import brbo.common.ast._

object ProgramTransformer {
  val USE_FUNCTION_NAME = "use"
  // This variable tracks the cost of every use command.
  // This variable decrements with the execution of any use command. When the variable equals 0, the program exits.
  private val indexVariable = Identifier(name = "INDEX_VARIABLE", typ = BrboType.INT)
  // This variable tracks the number of use commands that are executed per run.
  private val useCountVariable = Identifier(name = "USE_COUNT", typ = BrboType.INT)
  private val useVariable = Identifier(name = "USE", typ = BrboType.INT)
  private val returnUseVariable = Return(expression = Some(useVariable))
  private val specialVariables = List(indexVariable, useVariable, useCountVariable)

  def transform(program: BrboProgram, loopIterationMultiplier: Int, mode: DriverGenerator.Mode): BrboProgram = {
    val mainFunction = program.mainFunction
    val mainFunctionBody = mainFunction.body
    val useDefVariables = BrboAstUtils.collectUseDefVariables(mainFunction.bodyWithGhostInitialization)
    assert(useDefVariables.forall(identifier => !specialVariables.exists(anotherIdentifier => identifier.sameAs(anotherIdentifier))))

    // Replace all `R = R + e` with `use(e); index--; if (index == 0) return;` where use is a new function
    assert(program.allFunctions.forall(function => function.identifier != USE_FUNCTION_NAME))
    val replacements =
      BrboAstUtils.collectCommands(mainFunctionBody)
        .filter(command => command.isInstanceOf[Use] || command.isInstanceOf[Return])
        .map({
          case use: Use =>
            mode match {
              case DriverGenerator.Modified =>
                val newUse = generateUseAssignment(use)
                val returnIfEqual = ITE(Equal(indexVariable, useCountVariable), returnUseVariable, Skip())
                val incrementUseCountVariable = Assignment(useCountVariable, Addition(useCountVariable, Number(1)))
                (use.asInstanceOf[Command], Block(List(newUse, incrementUseCountVariable, returnIfEqual)))
              case DriverGenerator.Naive =>
                (use.asInstanceOf[Command], Comment(use.printToIR()))
              case _ => throw new Exception
            }
          case _return: Return =>
            (_return.asInstanceOf[Command], returnUseVariable)
        }).toMap
    val newBody = BrboAstUtils.replaceCommands(mainFunctionBody, replacements = replacements, omitResetPlaceHolders = false)

    val declarations = List(
      VariableDeclaration(useCountVariable, Number(0)),
      VariableDeclaration(useVariable, Number(0)),
    )
    val newBody2 = BrboAstUtils.insert(newBody, toInsert = declarations, operation = PrependOperation)
    val newBody3 = BrboAstUtils.insert(newBody2, toInsert = List(returnUseVariable), operation = AppendOperation)
    val newMainFunction = BrboFunction(
      identifier = mainFunction.identifier,
      returnType = BrboType.INT, // Return the number of uses in a run
      parameters = mainFunction.parameters :+ indexVariable,
      body = newBody3.asInstanceOf[Statement],
      groupIds = mainFunction.groupIds,
      useResource = mainFunction.useResource
    )
    BrboProgram(
      className = program.className,
      packageName = program.packageName,
      mainFunction = newMainFunction,
      boundAssertions = program.boundAssertions,
      otherFunctions = useFunction(loopIterationMultiplier) :: program.otherFunctions
    )
  }

  private def generateUseAssignment(use: Use): BrboAst = {
    val assignment = Assignment(useVariable, use.update)
    use.condition match {
      case Bool(true, _) => assignment
      case _ => ITE(use.condition, assignment, Skip())
    }
  }

  private def generateCallUse(use: Use): BrboAst = {
    val callUse = FunctionCallExpr(USE_FUNCTION_NAME, arguments = List(use.update), returnType = BrboType.VOID)
    use.condition match {
      case Bool(true, _) => callUse
      case _ => ITE(use.condition, callUse, Skip())
    }
  }

  private def useFunction(loopIterationMultiplier: Int) = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val declaration = VariableDeclaration(i, Number(0))
    val increment = Assignment(i, Addition(i, Number(1)))
    val loop = Loop(LessThan(i, Multiplication(n, Number(loopIterationMultiplier))), increment)
    BrboFunction(
      identifier = USE_FUNCTION_NAME,
      returnType = BrboType.VOID,
      parameters = List(n),
      body = Block(List(declaration, loop)),
      groupIds = Set(),
      useResource = false
    )
  }
}
