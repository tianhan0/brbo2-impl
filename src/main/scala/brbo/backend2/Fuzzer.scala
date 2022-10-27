package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.common.ast.{Bool, BrboArray, BrboProgram, BrboValue, Identifier, Number}
import brbo.common.{BrboType, MathUtils, MyLogger}

object Fuzzer {
  private val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode = false)

  def randomValues(typ: BrboType.T, maxArrayLength: Int, maxInteger: Int, possibilities: Int): List[BrboValue] = {
    typ match {
      case BrboType.INT =>
        val random = new scala.util.Random
        Range.inclusive(0, possibilities).toList.map(_ => Number(random.nextInt(maxInteger)))
      case BrboType.BOOL => List(Bool(b = false), Bool(b = true))
      case BrboType.ARRAY(typ) =>
        typ match {
          case BrboType.INT =>
            Range.inclusive(0, maxArrayLength).toList.flatMap({
              length =>
                MathUtils.crossJoin(
                  Range(0, length)
                    .toList
                    .map({ _ => randomValues(BrboType.INT, maxArrayLength, maxInteger, possibilities = 1) })
                ).map(list => BrboArray(list, BrboType.INT))
            })
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  def fuzz(brboProgram: BrboProgram, debugMode: Boolean): List[Interpreter.Trace] = {
    val interpreter = new Interpreter(brboProgram, debugMode)
    val inputs = MathUtils.crossJoin(brboProgram.mainFunction.parameters.map({
      case Identifier(_, typ, _) => randomValues(typ, maxArrayLength = 10, maxInteger = 1000, possibilities = 5)
    }))
    logger.info(s"Generated `${inputs.size}` inputs")
    inputs.map({
      inputValues =>
        val endState = interpreter.execute(inputValues)
        endState.trace
    })
  }
}
