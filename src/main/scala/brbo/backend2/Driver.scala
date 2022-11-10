package brbo.backend2

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.SegmentClustering.printSegments
import brbo.backend2.learning.{Classifier, SegmentClustering, TracePartition}
import brbo.common.ast._
import brbo.common.{BrboType, GhostVariableUtils, MyLogger, NewCommandLineArguments}

class Driver(arguments: NewCommandLineArguments, program: BrboProgram) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[Driver], debugMode)
  logger.info(s"Step 0: Insert reset place holders")
  private val instrumentedProgram = Driver.insertResetPlaceHolders(program)
  private val interpreter = new Interpreter(instrumentedProgram, debugMode)
  private val classifierFeatures = instrumentedProgram.mainFunction.nonGhostVariables().filter({
    variable =>
      variable.typ match {
        case BrboType.BOOL => true
        case BrboType.INT => !GhostVariableUtils.isGhostVariable(variable.name)
        case BrboType.ARRAY(_) => false
        case _ => false
      }
  })
  private val segmentClustering = new SegmentClustering(
    sumWeight = 1,
    commandWeight = 0,
    debugMode = debugMode,
    algorithm = arguments.getAlgorithm
  )

  def decompose(): BrboProgram = {
    logger.info(s"Step 1: Generate traces")
    val rawTraces = Fuzzer.fuzz(instrumentedProgram, debugMode, samples = arguments.getFuzzSamples)
    logger.info(s"Step 2: Select representative traces")
    val representatives = TracePartition.selectRepresentatives(rawTraces)
    logger.info(s"Step 3: Decompose ${representatives.size} selected traces")
    val decomposedPrograms = representatives.zipWithIndex.map({
      case ((traceRepresentative, similarTraces), index) =>
        decomposeTrace(traceRepresentative, index, similarTraces)
    })
    decomposedPrograms.head
  }

  private def decomposeTrace(trace: Trace, index: Int, similarTraces: Iterable[Trace]): BrboProgram = {
    logger.info(s"Step 3.1: Decompose $index-th representative trace")
    val representativeTraceString =
      if (debugMode)
        trace.toTable(printStores = true, omitExpressions = false, onlyGhostCommand = false)._1.printAll()
      else
        trace.toTable(printStores = false, omitExpressions = true, onlyGhostCommand = true)._1.printAll()
    logger.info(s"Step 3.1: Trace:\n$representativeTraceString")
    val decomposition = segmentClustering.decompose(trace, similarTraces, interpreter)
    val groups = decomposition.getGroups
    val groupsString = groups.map({
      case (groupID, group) => s"$groupID: ${printSegments(group.segments)}"
    }).mkString("\n")
    logger.info(s"Step 3.1: Selected decomposition:\n$groupsString\n${SegmentClustering.printDecomposition(trace, groups)}")
    logger.info(s"Step 3.2: Generate tables for training classifiers")
    val tables = Classifier.generateTables(
      trace,
      Classifier.evaluateFunctionFromInterpreter(interpreter),
      groups,
      features = classifierFeatures,
      failIfCannotFindResetPlaceHolder = false
    )
    // logger.traceOrError(s"Step 3.2: Generated tables:\n${tables.print()}")
    logger.info(s"Step 3.3: Generate classifiers on the tables")
    val classifierResults = tables.toProgramTables.generateClassifiers(debugMode)
    logger.info(s"Step 3.4: Generate program transformations")
    val transformation: Map[BrboAst, BrboAst] = classifierResults.toTransformation
    logger.info(s"Step 3.4: See below for a mapping from existing ASTs to new ASTs")
    transformation.foreach({
      case (oldAst, newAst) =>
        logger.info(s"${oldAst.asInstanceOf[Command].printToIR()} -> ${newAst.printToC(0)}")
    })
    val ghostVariableIDs = {
      transformation.flatMap({
        case (_, ast1) =>
          BrboAstUtils.collectCommands(ast1).flatMap({
            case use: Use => use.groupId
            case reset: Reset => Some(reset.groupId)
            case _ => None
          })
      })
    }
    val newBody = BrboAstUtils.replaceAst(instrumentedProgram.mainFunction.body, transformation)
    val newMain = {
      val main = instrumentedProgram.mainFunction
      BrboFunction(main.identifier, main.returnType, main.parameters, newBody.asInstanceOf[Statement], ghostVariableIDs.toSet)
    }
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
