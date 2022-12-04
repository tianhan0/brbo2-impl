package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.common.ast.{Bool, BrboArray, BrboAstUtils, BrboProgram, BrboValue, Identifier, Number}
import brbo.common.string.StringFormatUtils
import brbo.common.{BrboType, MathUtils, MyLogger}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Fuzzer(maxInteger: Int, minInteger: Int) {
  private def randomInteger(samples: Int, seed: Int): List[BrboValue] = {
    val random = new scala.util.Random(seed)
    Range.inclusive(0, samples).toList.map(_ => Number(minInteger + random.nextInt(maxInteger - minInteger + 1)))
  }

  private def randomArray(samples: Int, maxArrayLength: Int): List[BrboValue] = {
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
              elementID => randomInteger(samples = 1, seed = partitionSize + length + sampleID + elementID).head
            }).toList
            BrboArray(array, BrboType.INT)
        })
    })
  }

  def randomValues(typ: BrboType.T, samples: Int, maxArrayLength: Int, seed: Int): List[BrboValue] = {
    typ match {
      case BrboType.INT => randomInteger(samples, seed)
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
            randomArray(samples, maxArrayLength)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
  }
}

object Fuzzer {
  val DEFAULT_SAMPLES = 0
  private val MAX_ARRAY_LENGTH = 5
  private val MAX_INTEGER = 300 // No need to be a huge number. Just need to let the costs vary.
  private val MIN_INTEGER = 10
  val LOOP_ITERATIONS: List[Int] = List(2, 5, 8)

  def fuzz(brboProgram: BrboProgram, debugMode: Boolean, samples: Int): List[Interpreter.Trace] = {
    val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode)
    val FUZZING = s"Fuzzing: ${brboProgram.name}: "
    val interpreter = new Interpreter(brboProgram, debugMode)
    val fuzzer = new Fuzzer(maxInteger = MAX_INTEGER, minInteger = MIN_INTEGER)
    // val loopIterationsFuzzer = new Fuzzer(maxInteger = 53, minInteger = 2)
    val loopConditionals = BrboAstUtils.getLoopConditionals(brboProgram.mainFunction.body)
    val inputs = MathUtils.crossJoin(brboProgram.mainFunction.parameters.zipWithIndex.map({
      case (identifier@Identifier(_, typ, _), index) =>
        val usedInLoopConditionals = loopConditionals.exists({
          e => e.getUses.contains(identifier)
        })
        if (!usedInLoopConditionals)
          fuzzer.randomValues(typ, samples, maxArrayLength = MAX_ARRAY_LENGTH, seed = index)
        else {
          // loopIterationsFuzzer.randomValues(typ, samples = LOOP_ITERATIONS_SAMPLE, maxArrayLength = MAX_ARRAY_LENGTH, seed = index)
          LOOP_ITERATIONS.map(i => Number(i))
        }
    }))
    logger.info(s"${FUZZING}Generated `${inputs.size}` inputs")

    val random = new scala.util.Random(seed = 12662)
    val futures = Future.traverse(inputs.zipWithIndex)({
      case (inputValues, index) =>
        Future {
          if (index % (1 + random.nextInt(50)) == 0) {
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
