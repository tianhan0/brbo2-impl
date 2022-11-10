package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.common.ast.{Bool, BrboArray, BrboProgram, BrboValue, Identifier, Number}
import brbo.common.string.StringFormatUtils
import brbo.common.{BrboType, MathUtils, MyLogger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Fuzzer {
  val SAMPLES = 3
  private val MAX_ARRAY_LENGTH = 5
  private val MAX_INTEGER = 100

  private def randomInteger(samples: Int, maxInteger: Int, seed: Int): List[BrboValue] = {
    val random = new scala.util.Random(seed)
    Range.inclusive(0, samples).toList.map(_ => Number(random.nextInt(maxInteger)))
  }

  private def randomArray(samples: Int, maxArrayLength: Int, maxInteger: Int): List[BrboValue] = {
    val partitionSize = {
      val partitionSize = samples / maxArrayLength
      if (partitionSize == 0) 1
      else partitionSize
    }
    Range.inclusive(1, maxArrayLength + 1).toList.flatMap({
      length =>
        Range(0, partitionSize).map({
          sampleID =>
            // seed only depends on `samples` and `maxArrayLength`
            val array = Range(0, length).map({
              elementID => randomInteger(samples = 1, maxInteger, seed = partitionSize + length + sampleID + elementID).head
            }).toList
            BrboArray(array, BrboType.INT)
        })
    })
  }

  def randomValues(typ: BrboType.T, samples: Int, maxArrayLength: Int, maxInteger: Int): List[BrboValue] = {
    typ match {
      case BrboType.INT => randomInteger(samples, maxInteger, seed = 62618)
      case BrboType.BOOL => List(Bool(b = false), Bool(b = true))
      case BrboType.ARRAY(typ) =>
        typ match {
          case BrboType.INT =>
            /*Range.inclusive(0, maxArrayLength).toList.flatMap({
              length =>
                MathUtils.crossJoin(
                  Range(0, length)
                    .toList
                    .map({ _ => randomValues(BrboType.INT, maxArrayLength, maxInteger, samples = 3) })
                ).map(list => BrboArray(list, BrboType.INT))
            })*/
            randomArray(samples, maxArrayLength, maxInteger)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }

  def fuzz(brboProgram: BrboProgram, debugMode: Boolean, samples: Int): List[Interpreter.Trace] = {
    val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode)
    val FUZZING = s"Fuzzing program ${brboProgram.name}: "
    val interpreter = new Interpreter(brboProgram, debugMode)
    val inputs = MathUtils.crossJoin(brboProgram.mainFunction.parameters.map({
      case Identifier(_, typ, _) => randomValues(typ, samples, maxArrayLength = MAX_ARRAY_LENGTH, maxInteger = MAX_INTEGER)
    }))
    logger.info(s"${FUZZING}Generated `${inputs.size}` inputs")

    val futures = Future.traverse(inputs.zipWithIndex)({
      case (inputValues, index) =>
        Future {
          if (index % 500 == 0) {
            val percentage = StringFormatUtils.float(index.toDouble / inputs.size * 100, digit = 2)
            logger.info(s"$FUZZING$index / ${inputs.size} ($percentage%)")
            logger.info(s"Inputs: ${inputValues.map(v => v.printToIR())}")
          }
          logger.traceOrError(s"Inputs: ${inputValues.map(v => v.printToIR())}")
          val endState = interpreter.execute(inputValues)
          // logger.traceOrError(s"Trace:\n${endState.trace.toTable.printAll()}")
          endState.trace
        }
    })
    Await.result(futures, Duration.Inf)
  }
}
