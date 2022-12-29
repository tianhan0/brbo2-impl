package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.ast.BrboAstUtils.{Append, Prepend}
import brbo.common.ast._

object ProgramTransformer {
  val USE_FUNCTION_NAME = "use"
  // This variable tracks the cost of every use command.
  // This variable decrements with the execution of any use command. When the variable equals 0, the program exits.
  private val indexVariable = Identifier(name = "INDEX_VARIABLE", typ = BrboType.INT)
  // This variable tracks the number of use commands that are executed per run.
  private val useCountVariable = Identifier(name = "USE_COUNT", typ = BrboType.INT)
  private val returnUseCountVariable = Return(expression = Some(useCountVariable))
  private val specialVariables = List(indexVariable, useCountVariable)

  private val MAX_LOOP_ITERATIONS = 8

  def transform(program: BrboProgram): BrboProgram = {
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
            val newUse = generateCallUse(use)
            val incrementUseCountVariable = Assignment(useCountVariable, Addition(useCountVariable, Number(1)))
            val decrementIndexVariable = Assignment(indexVariable, Subtraction(indexVariable, Number(1)))
            val returnIfZero = ITE(Equal(indexVariable, Number(0)), returnUseCountVariable, Skip())
            (use.asInstanceOf[Command], Block(List(newUse, incrementUseCountVariable, decrementIndexVariable, returnIfZero)))
          case _return: Return =>
            (_return.asInstanceOf[Command], returnUseCountVariable)
        }).toMap
    val newBody = BrboAstUtils.replaceCommands(mainFunctionBody, replacements = replacements, omitResetPlaceHolders = false)

    // Early return if any variable used in loop conditions is beyond `MAX_LOOP_ITERATIONS`, to avoid executing a loop
    // for too many times, which makes it expensive to choose segments.
    val loopConditionals = BrboAstUtils.getLoopConditionals(mainFunctionBody)
    val earlyReturns: List[BrboAst] = mainFunction.parameters.filter({
      identifier => identifier.typ == BrboType.INT && loopConditionals.exists({ e => e.getUses.contains(identifier) })
    }).map({
      identifier => ITE(LessThan(Number(MAX_LOOP_ITERATIONS), identifier), returnUseCountVariable, Skip())
    })

    val newBody2 = BrboAstUtils.insert(newBody, toInsert = VariableDeclaration(useCountVariable, Number(0)) :: earlyReturns, operation = Prepend)
    val newBody3 = BrboAstUtils.insert(newBody2, toInsert = List(returnUseCountVariable), operation = Append)
    val newMainFunction = BrboFunction(
      identifier = mainFunction.identifier,
      returnType = BrboType.INT, // Return the number of uses in a run
      parameters = mainFunction.parameters :+ indexVariable,
      body = newBody3.asInstanceOf[Statement],
      groupIds = mainFunction.groupIds
    )
    BrboProgram(
      className = program.className,
      packageName = program.packageName,
      mainFunction = newMainFunction,
      boundAssertions = program.boundAssertions,
      otherFunctions = useFunction :: program.otherFunctions
    )
  }

  private def generateCallUse(use: Use): BrboAst = {
    val callUse = FunctionCallExpr(USE_FUNCTION_NAME, arguments = List(use.update), returnType = BrboType.VOID)
    use.condition match {
      case Bool(true, _) => callUse
      case _ => ITE(use.condition, callUse, Skip())
    }
  }

  private val useFunction = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val declaration = VariableDeclaration(i, Number(0))
    val increment = Assignment(i, Addition(i, Number(1)))
    val loop = Loop(LessThan(i, Multiplication(n, Number(1000))), increment)
    BrboFunction(
      identifier = USE_FUNCTION_NAME,
      returnType = BrboType.VOID,
      parameters = List(n),
      body = Block(List(declaration, loop)),
      groupIds = Set()
    )
  }
}
