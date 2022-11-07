package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.common.ast.{Bool, BrboArray, BrboProgram, BrboValue, Identifier, Number}
import brbo.common.string.StringFormatUtils
import brbo.common.{BrboType, MathUtils, MyLogger}

object Fuzzer {
  private val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode = false)
  private val POSSIBILITIES = 3
  private val MAX_ARRAY_LENGTH = 10
  private val MAX_INTEGER = 1000
  private val SEED = 62618L

  def randomValues(typ: BrboType.T, maxArrayLength: Int, maxInteger: Int, possibilities: Int): List[BrboValue] = {
    typ match {
      case BrboType.INT =>
        val random = new scala.util.Random(seed = SEED)
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
    val FUZZING = s"Fuzzing program ${brboProgram.name}: "
    val interpreter = new Interpreter(brboProgram, debugMode)
    val inputs = MathUtils.crossJoin(brboProgram.mainFunction.parameters.map({
      case Identifier(_, typ, _) => randomValues(typ, maxArrayLength = MAX_ARRAY_LENGTH, maxInteger = MAX_INTEGER, possibilities = POSSIBILITIES)
    }))
    logger.info(s"${FUZZING}Generated `${inputs.size}` inputs")
    inputs.zipWithIndex.map({
      case (inputValues, index) =>
        if (index % 10000 == 0) {
          val percentage = StringFormatUtils.float(index.toDouble / inputs.size * 100, digit = 2)
          logger.info(s"$FUZZING$index / ${inputs.size} ($percentage%)")
        }
        val endState = interpreter.execute(inputValues)
        endState.trace
    })
  }
}
