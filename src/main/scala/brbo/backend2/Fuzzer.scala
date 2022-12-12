package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.common.ast._
import brbo.common.string.StringFormatUtils
import brbo.common.{BrboType, MyLogger}

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

class Fuzzer(maxInteger: Int, minInteger: Int, maxArrayLength: Int, minArrayLength: Int) {
  def randomValues(typ: BrboType.T, samples: Int, seed: Int): List[BrboValue] = {
    val result = typ match {
      case BrboType.INT => randomInteger(samples, seed)
      case BrboType.BOOL => randomBoolean(samples, seed)
      case BrboType.ARRAY(typ) =>
        typ match {
          case BrboType.INT => randomIntegerArray(samples, seed)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
    assert(result.size == samples)
    result
  }

  private def randomInteger(samples: Int, seed: Int): List[BrboValue] = {
    Range(0, samples).toList.map({
      sampleIndex =>
        val random = new scala.util.Random(seed + sampleIndex)
        Number(nextRandomInteger(random, min = minInteger, max = maxInteger))
    })
  }

  private def randomBoolean(samples: Int, seed: Int): List[BrboValue] = {
    val random = new scala.util.Random(seed)
    val average = (maxInteger + minInteger) / 2
    Range(0, samples).toList.map({
      _ => Bool(b = nextRandomInteger(random, min = minInteger, max = maxInteger) >= average)
    })
  }

  private def nextRandomInteger(random: Random, min: Int, max: Int): Int = min + random.nextInt(max - min + 1)

  private def randomIntegerArray(samples: Int, seed: Int): List[BrboValue] = {
    val random = new scala.util.Random(seed)
    Range(0, samples).map({
      sampleIndex =>
        val length = nextRandomInteger(random, min = minArrayLength, max = maxArrayLength)
        val array = randomInteger(samples = length, seed = seed * (length + 1) + sampleIndex)
        // println(s"$seed, $length, $sampleIndex ${array.map(v => v.printToIR())}")
        BrboArray(array, BrboType.INT)
    }).toList
  }
}

object Fuzzer {
  val DEFAULT_SAMPLES = 0
  private val MIN_ARRAY_LENGTH = 3
  private val MAX_ARRAY_LENGTH = 4
  private val MAX_INTEGER = 300 // No need to be a huge number. Just need to let the costs vary.
  private val MIN_INTEGER = 10
  val SEED: Int = 6182
  val LOOP_ITERATIONS: List[Int] = List(2, 5, 8)
  private val fuzzer = new Fuzzer(
    maxInteger = MAX_INTEGER, minInteger = MIN_INTEGER,
    maxArrayLength = MAX_ARRAY_LENGTH, minArrayLength = MIN_ARRAY_LENGTH)

  def fuzz(brboProgram: BrboProgram, debugMode: Boolean, samples: Int, threads: Int): List[Interpreter.Trace] = {
    val executionContextExecutor = {
      val executorService = Executors.newFixedThreadPool(threads)
      ExecutionContext.fromExecutor(executorService)
    }

    val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode)
    val FUZZING = s"Fuzzing: ${brboProgram.name}: "
    val interpreter = new Interpreter(brboProgram, debugMode)
    val loopConditionals = BrboAstUtils.getLoopConditionals(brboProgram.mainFunction.body)
    val inputs = generateInputs(brboProgram.mainFunction.parameters, samples, loopConditionals)
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
        }(executionContextExecutor)
    })(implicitly, executionContextExecutor)
    Await.result(futures, Duration.Inf)
  }

  private def generateInputs(inputs: List[Identifier], samples: Int, loopConditionals: Set[BrboExpr]): List[List[BrboValue]] = {
    val possibleValues = inputs.zipWithIndex.map({
      case (identifier, index) =>
        val usedInLoopConditionals = loopConditionals.exists({ e => e.getUses.contains(identifier) })
        // If using the same seed below, then we will generate the same possible values for all inputs
        val values = fuzzer.randomValues(identifier.typ, samples, seed = index)
        if (usedInLoopConditionals && identifier.typ == BrboType.INT) {
          values.map({
            case Number(n, _) => Number(LOOP_ITERATIONS(n % LOOP_ITERATIONS.size))
            case _ => throw new Exception
          })
        } else {
          values
        }
    })
    var result: List[List[BrboValue]] = Nil
    Range(0, samples).foreach({
      i =>
        val list: List[BrboValue] = Range(0, inputs.size).map(index => possibleValues(index)(i)).toList
        result = list :: result
    })
    result
  }
}
