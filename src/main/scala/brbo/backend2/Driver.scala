package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.{Classifier, SegmentClustering, TracePartition}
import brbo.common.ast.{BrboAst, BrboAstUtils, BrboProgram, Statement}
import brbo.common.{MyLogger, NewCommandLineArguments}

class Driver(arguments: NewCommandLineArguments, program: BrboProgram) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[Driver], debugMode)
  logger.info(s"Step 0: Insert reset place holders")
  private val instrumentedProgram = Driver.insertResetPlaceHolders(program)
  private val interpreter = new Interpreter(instrumentedProgram, debugMode)
  private val classifierFeatures = instrumentedProgram.mainFunction.nonGhostVariables()
  private val segmentClustering = new SegmentClustering(
    sumWeight = 1,
    commandWeight = 0,
    debugMode = debugMode,
    algorithm = arguments.getAlgorithm
  )

  def decompose(): BrboProgram = {
    logger.info(s"Step 1: Generate traces")
    val rawTraces = Fuzzer.fuzz(program, debugMode, samples = arguments.getFuzzSamples)
    logger.info(s"Step 2: Select representative traces")
    val representatives = TracePartition.selectRepresentatives(rawTraces)
    logger.info(s"Step 3: Decompose selected traces")
    val decomposedPrograms = representatives.zipWithIndex.map({
      case (traceRepresentative, index) =>
        decomposeTrace(traceRepresentative, index, similarTraces = rawTraces)
    })
    decomposedPrograms.head
  }

  private def decomposeTrace(trace: Trace, index: Int, similarTraces: Iterable[Trace]): BrboProgram = {
    logger.info(s"Step 3.1: Decompose $index-th representative trace")
    val decomposition = segmentClustering.decompose(trace, similarTraces, interpreter)
    logger.traceOrError(s"Selected decomposition:\n${SegmentClustering.printDecomposition(trace, decomposition.getGroups)}")
    val groups = decomposition.getGroups
    logger.info(s"Step 3.2: Generate tables for training classifiers")
    val tables = Classifier.generateTables(
      trace,
      Classifier.evaluateFunctionFromInterpreter(interpreter),
      groups,
      features = classifierFeatures,
      failIfCannotFindResetPlaceHolder = false
    )
    if (debugMode)
      logger.info(s"Generated tables:\n${tables.print()}")
    logger.info(s"Step 3.3: Generate classifiers on the tables")
    val classifierResults = tables.toProgramTables.generateClassifiers(debugMode)
    logger.info(s"Step 3.4: Generate program transformations")
    val transformation: Map[BrboAst, BrboAst] = classifierResults.toTransformation
    val newBody = BrboAstUtils.replaceAst(instrumentedProgram.mainFunction.body, transformation)
    val newMain = instrumentedProgram.mainFunction.replaceBodyWithoutInitialization(newBody.asInstanceOf[Statement])
    instrumentedProgram.replaceMainFunction(newMain)
  }
}

object Driver {
  def insertResetPlaceHolders(program: BrboProgram): BrboProgram = {
    val mainFunctionWithResetPlaceHolders =
      program.mainFunction.replaceBodyWithoutInitialization(
        BrboAstUtils.insertResetPlaceHolder(program.mainFunction.body).asInstanceOf[Statement]
      )
    program.replaceMainFunction(mainFunctionWithResetPlaceHolders)
  }
}
