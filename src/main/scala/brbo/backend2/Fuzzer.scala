package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.qfuzz.DriverGenerator
import brbo.backend2.qfuzz.DriverGenerator.{MAX_LOOP_ITERATIONS, MIN_LOOP_ITERATIONS}
import brbo.common.ast._
import brbo.common.{BrboType, MyLogger}

import scala.util.Random

class Fuzzer(maxInteger: Int, minInteger: Int, maxArrayLength: Int, minArrayLength: Int) {
  def randomValues(typ: BrboType.T, samples: Int, seed: Int, deterministic: Boolean): List[BrboValue] = {
    val result = typ match {
      case BrboType.INT => randomInteger(samples, seed, deterministic)
      case BrboType.BOOL => randomBoolean(samples, seed, deterministic)
      case BrboType.ARRAY(typ) =>
        typ match {
          case BrboType.INT => randomIntegerArray(samples, seed, deterministic)
          case _ => throw new Exception
        }
      case _ => throw new Exception
    }
    assert(result.size == samples)
    result
  }

  private def randomInteger(samples: Int, seed: Int, deterministic: Boolean): List[BrboValue] = {
    Range(0, samples).toList.map({
      sampleIndex =>
        val random = if (deterministic) new Random(seed + sampleIndex) else new Random()
        Number(nextRandomInteger(random, min = minInteger, max = maxInteger))
    })
  }

  private def randomBoolean(samples: Int, seed: Int, deterministic: Boolean): List[BrboValue] = {
    val random = if (deterministic) new Random(seed) else new Random()
    val average = (maxInteger + minInteger) / 2
    Range(0, samples).toList.map({
      _ => Bool(b = nextRandomInteger(random, min = minInteger, max = maxInteger) >= average)
    })
  }

  private def nextRandomInteger(random: Random, min: Int, max: Int): Int = min + random.nextInt(max - min + 1)

  private def randomIntegerArray(samples: Int, seed: Int, deterministic: Boolean): List[BrboValue] = {
    val random = if (deterministic) new Random(seed) else new Random()
    Range(0, samples).map({
      sampleIndex =>
        val length = nextRandomInteger(random, min = minArrayLength, max = maxArrayLength)
        val array = randomInteger(samples = length, seed = seed * (length + 1) + sampleIndex, deterministic)
        // println(s"$seed, $length, $sampleIndex ${array.map(v => v.printToIR())}")
        BrboArray(array, BrboType.INT)
    }).toList
  }
}

object Fuzzer {
  val DEFAULT_SAMPLES = 0
  private val MIN_ARRAY_LENGTH = DriverGenerator.ARRAY_SIZE
  private val MAX_ARRAY_LENGTH = DriverGenerator.ARRAY_SIZE
  private val MAX_INTEGER = 50 // TODO: Use the user-provided values
  private val MIN_INTEGER = 4 // TODO: Use the user-provided values
  val SEED: Int = 6182
  private val fuzzer = new Fuzzer(
    maxInteger = MAX_INTEGER,
    minInteger = MIN_INTEGER,
    maxArrayLength = MAX_ARRAY_LENGTH,
    minArrayLength = MIN_ARRAY_LENGTH
  )

  def fuzz(brboProgram: BrboProgram,
           interpreter: Interpreter,
           debugMode: Boolean,
           samples: Int,
           threads: Int): List[Interpreter.Trace] = {
    val logger = MyLogger.createLogger(Fuzzer.getClass, debugMode)
    val loopConditionals = BrboAstUtils.getLoopConditionals(brboProgram.mainFunction.body)
    val inputs = generateInputs(brboProgram.mainFunction.parameters, samples, loopConditionals)
    logger.info(s"[Fuzzing ${brboProgram.className}] Generated `${inputs.size}` inputs")
    Interpreter.generateTraces(inputs, interpreter, threads, debugMode)
  }

  private def generateInputs(inputs: List[Identifier], samples: Int, loopConditionals: Set[BrboExpr]): List[List[BrboValue]] = {
    val possibleValues = inputs.zipWithIndex.map({
      case (identifier, index) =>
        val usedInLoopConditionals = loopConditionals.exists({ e => e.getUses.contains(identifier) })
        // If using the same seed below, then we will generate the same possible values for all inputs
        val values = fuzzer.randomValues(identifier.typ, samples, seed = index, deterministic = false)
        if (usedInLoopConditionals && identifier.typ == BrboType.INT) {
          values.map({
            case Number(n, _) =>
              val loopIterations = n % (MAX_LOOP_ITERATIONS - MIN_LOOP_ITERATIONS + 1) + MIN_LOOP_ITERATIONS;
              Number(loopIterations)
            case _ => throw new Exception
          })
        } else {
          values
        }
    })
    var result: List[List[BrboValue]] = Nil
    Range(0, samples).foreach({
      i =>
        val list: List[BrboValue] = inputs.indices.map(index => possibleValues(index)(i)).toList
        result = list :: result
    })
    result
  }
}
