package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.ast._

object ProgramTransformer {
  def transform(program: BrboProgram): BrboProgram = {
    val mainFunction = program.mainFunction
    val mainFunctionBody = mainFunction.body
    val useDefVariables = BrboAstUtils.collectUseDefVariables(mainFunction.bodyWithGhostInitialization)
    // Generate a variable that decrements with the execution of any use command. When the variable equals 0, the program exits.
    // This variable is used to track the cost of every use command.
    val indexVariable = generateIndexVariable(useDefVariables)

    // Replace all `R = R + e` with `use(e); index--; if (index == 0) return;` where use is a new function
    assert(program.allFunctions.forall(function => function.identifier != USE_FUNCTION_NAME))
    val replacements = BrboAstUtils.collectCommands(mainFunctionBody).filter(command => command.isInstanceOf[Use]).map({
      case use: Use =>
        val newUse = generateCallUse(use)
        val decrementIndexVariable = Assignment(indexVariable, Subtraction(indexVariable, Number(1)))
        val returnIfZero = ITE(Equal(indexVariable, Number(0)), Return(expression = None), Skip())
        (use.asInstanceOf[Command], Block(List(newUse, decrementIndexVariable, returnIfZero)))
    }).toMap
    val newBody = BrboAstUtils.replaceCommands(mainFunctionBody, replacements = replacements, omitResetPlaceHolders = false)

    // Early return if any variable used in loop conditions is beyond `MAX_LOOP_ITERATIONS`, to avoid executing a loop
    // for too many times, which makes it expensive to choose segments.
    val loopConditionals = BrboAstUtils.getLoopConditionals(mainFunctionBody)
    val earlyReturns: Iterable[BrboAst] = mainFunction.parameters.filter({
      identifier => identifier.typ == BrboType.INT && loopConditionals.exists({ e => e.getUses.contains(identifier) })
    }).map({
      identifier => ITE(LessThan(Number(MAX_LOOP_ITERATIONS), identifier), Return(expression = None), Skip())
    })

    val newBody2 = BrboAstUtils.prepend(newBody, toPrepend = earlyReturns)
    val newMainFunction = BrboFunction(
      identifier = mainFunction.identifier,
      returnType = mainFunction.returnType,
      parameters = mainFunction.parameters :+ indexVariable,
      body = newBody2.asInstanceOf[Statement],
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

  val USE_FUNCTION_NAME = "use"
  private val INDEX_VARIABLE_NAME = "INDEX_VARIABLE"
  private val MAX_LOOP_ITERATIONS = 8

  private def generateCallUse(use: Use): BrboAst = {
    val callUse = FunctionCallExpr(USE_FUNCTION_NAME, arguments = List(use.update), returnType = BrboType.VOID)
    use.condition match {
      case Bool(true, _) => callUse
      case _ => ITE(use.condition, callUse, Skip())
    }
  }

  private def generateIndexVariable(useDefVariables: Iterable[Identifier]): Identifier = {
    assert(useDefVariables.forall(identifier => identifier.name != INDEX_VARIABLE_NAME))
    Identifier(name = INDEX_VARIABLE_NAME, typ = BrboType.INT)
  }

  private val useFunction = {
    val i = Identifier("i", BrboType.INT)
    val n = Identifier("n", BrboType.INT)
    val declaration = VariableDeclaration(i, Number(0))
    val increment = Assignment(i, Addition(i, Number(1)))
    val loop = Loop(LessThan(i, n), increment)
    BrboFunction(
      identifier = USE_FUNCTION_NAME,
      returnType = BrboType.VOID,
      parameters = List(n),
      body = Block(List(declaration, loop)),
      groupIds = Set()
    )
  }
}
