package brbo.backend2

import brbo.BrboMain
import brbo.backend2.DecompositionDriver._
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.Classifier.{GroupID, TraceTables}
import brbo.backend2.learning.SegmentClustering.printSegments
import brbo.backend2.learning._
import brbo.common.ast._
import brbo.common.cfg.ControlFlowGraph
import brbo.common.commandline.DecompositionArguments
import brbo.common.{BrboType, GhostVariableUtils, MyLogger}
import org.apache.commons.io.FilenameUtils

import java.nio.file.{Files, Paths}

class DecompositionDriver(arguments: DecompositionArguments,
                          program: BrboProgram,
                          userProvidedInputFile: Option[String],
                          qfuzzInputFile: String) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[DecompositionDriver], debugMode)
  logger.info(s"Step 0: Insert reset place holders")
  private val instrumentedProgram = DecompositionDriver.insertResetPlaceHolders(program)
  private val interpreter = new Interpreter(instrumentedProgram, debugMode)
  private val features = DecompositionDriver.classifierFeatures(instrumentedProgram)
  logger.info(s"Step 0: Find classifier features {${features.map(i => i.printToIR()).mkString(", ")}}")
  private val segmentClustering = new SegmentClustering(
    sumWeight = 1,
    commandWeight = 0,
    debugMode = debugMode,
    algorithm = arguments.getAlgorithm,
    threads = arguments.getThreads,
  )

  def decompose(): BrboProgram = {
    logger.info(s"Step 1: Generate traces")
    val userProvidedTraces: List[Trace] = {
      if (arguments.getUseProvidedInputs && userProvidedInputFile.isDefined)
        generateTraces(userProvidedInputFile.get)
      else
        Nil
    }
    logger.info(s"Step 1.1: User-provided inputs (size ${userProvidedTraces.size})")
    val fuzzerGeneratedTraces: List[Trace] = {
      Fuzzer.fuzz(
        brboProgram = instrumentedProgram,
        interpreter = interpreter,
        debugMode = debugMode,
        samples = arguments.getFuzzSamples,
        threads = arguments.getThreads
      )
    }
    logger.info(s"Step 1.2: Fuzzer generated inputs (size ${fuzzerGeneratedTraces.size})")
    val qfuzzGeneratedTraces: List[Trace] = {
      logger.info(s"Step 1.2: Attempt to read from $qfuzzInputFile")
      if (Files.exists(Paths.get(qfuzzInputFile)))
        generateTraces(qfuzzInputFile)
      else
        Nil
    }
    logger.info(s"Step 1.3: QFuzz generated inputs (size ${qfuzzGeneratedTraces.size})")

    logger.info(s"Step 2: Cluster traces")
    val inputTraces = selectTraces(
      userProvidedTraces = userProvidedTraces,
      fuzzerGeneratedTraces = fuzzerGeneratedTraces,
      qfuzzGeneratedTraces = qfuzzGeneratedTraces,
    )
    val selectedTraces = inputTraces match {
      case DecompositionDriver.FuzzerGeneratedTrace => fuzzerGeneratedTraces
      case DecompositionDriver.QFuzzGeneratedTrace => qfuzzGeneratedTraces
      case DecompositionDriver.UserProvidedTrace => userProvidedTraces
      case _ => throw new Exception
    }
    val clusteredTraces: List[List[Trace]] = TraceClustering.run(
      traces = selectedTraces,
      debugMode = debugMode,
      stepId = 3,
      preserveTraceOrder = true
    )
    val traceTables: List[TraceTables] = clusteredTraces.zipWithIndex.map({
      case (cluster, index) =>
        logger.info(s"Step 3: Select representative traces for cluster $index")
        val representatives = TraceSelection.selectRepresentatives(
          traces = cluster,
          inputTrace = inputTraces,
        )
        val traceTables = representatives.zipWithIndex.map({
          case ((traceRepresentative, similarTraces), index) =>
            decomposeTrace(traceRepresentative, index, similarTraces, stepId = 4),
        })
        traceTables.head
    })
    generalize(traceTables, stepId = 4)
  }

  private def selectTraces(userProvidedTraces: List[Trace],
                           fuzzerGeneratedTraces: List[Trace],
                           qfuzzGeneratedTraces: List[Trace]): InputTrace = {
    if (userProvidedTraces.nonEmpty) {
      logger.info(s"Select representatives from user-provided traces")
      return UserProvidedTrace
    }
    if (qfuzzGeneratedTraces.nonEmpty) {
      logger.info(s"Select representatives from qfuzz generated traces")
      return QFuzzGeneratedTrace
    }
    logger.info(s"Select representatives from fuzzer generated traces")
    FuzzerGeneratedTrace
  }

  private def generateTraces(inputFilePath: String): List[Trace] = {
    val inputFileContents = BrboMain.readFromFile(inputFilePath)
    val inputs = InputParser.parse(inputFileContents)
    Interpreter.generateTraces(
      inputs = inputs,
      interpreter = interpreter,
      threads = arguments.getThreads,
      debugMode = debugMode
    )
  }

  private def decomposeTrace(trace: Trace, index: Int, similarTraces: Iterable[Trace], stepId: Int): TraceTables = {
    logger.info(s"Step $stepId.1: Decompose $index-th representative trace")
    val representativeTraceString =
      if (debugMode)
        trace.toTable(printStores = true, omitExpressions = true, onlyGhostCommand = false)._1.printAll()
      else
        trace.toTable(printStores = false, omitExpressions = true, onlyGhostCommand = true)._1.printAll()
    logger.info(s"Step $stepId.1: Trace (length ${trace.length}, inputs <${trace.printInputsOneLiner()}>):\n$representativeTraceString")
    val decomposition = segmentClustering.decompose(
      trace = trace,
      similarTraces = similarTraces,
      interpreter = interpreter,
      features = features,
      stepId = stepId + 0.1,
    )
    val groups = decomposition.getGroups
    val groupsString = groups.map({
      case (groupID, group) => s"$groupID: ${printSegments(group.segments)}"
    }).mkString("\n")
    logger.info(s"Step $stepId.1: Selected decomposition:\n$groupsString\n${SegmentClustering.printDecomposition(trace, groups)}")
    logger.info(s"Step $stepId.2: (TODO) Select reset locations for the groups")
    val resetPlaceHolderIndices: Map[GroupID, Set[Int]] = ResetPlaceHolderFinder.indices(
      trace = trace,
      groups = groups,
      controlFlowGraph = ControlFlowGraph.toControlFlowGraph(instrumentedProgram),
      throwIfNoResetPlaceHolder = arguments.getThrowIfNoResetPlaceHolder
    )
    logger.info(s"Step $stepId.3: Generate tables for training classifiers")
    val traceTables = Classifier.generateTables(
      trace = trace,
      evaluate = Classifier.evaluateFromInterpreter(interpreter),
      groups = groups,
      features = features,
      resetPlaceHolderIndices = resetPlaceHolderIndices,
    )
    // logger.traceOrError(s"Step $stepId.2: Generated tables:\n${traceTables.print()}")
    traceTables
  }

  private def generalize(traceTables: Iterable[TraceTables], stepId: Int): BrboProgram = {
    logger.info(s"Step $stepId.1: Generate classifiers on the tables")
    val programTables = Classifier.toProgramTables(traceTables.flatMap({ traceTable => traceTable.tables }).toMap, traceTables.head.features)
    val classifierResults = programTables.generateClassifiers(debugMode)
    logger.info(s"Step $stepId.2: Generate program transformations")
    val transformation: Map[Command, BrboAst] = classifierResults.toTransformation
    logger.info(Classifier.printTransformation(transformation))
    val newBody = BrboAstUtils.replaceCommands(instrumentedProgram.mainFunction.body, transformation, omitResetPlaceHolders = true)
    val newMain = {
      val ghostVariableIDs = {
        transformation.flatMap({
          case (_, ast) =>
            BrboAstUtils.collectCommands(ast).flatMap({
              case use: Use => use.groupId
              case reset: Reset => Some(reset.groupId)
              case _ => None
            })
        })
      }
      val main = instrumentedProgram.mainFunction
      BrboFunction(main.identifier, main.returnType, main.parameters, newBody.asInstanceOf[Statement], ghostVariableIDs.toSet, useResource = main.useResource)
    }
    instrumentedProgram.replaceMainFunction(newMain)
  }
}

object DecompositionDriver {
  abstract class InputTrace

  object UserProvidedTrace extends InputTrace

  object FuzzerGeneratedTrace extends InputTrace

  object QFuzzGeneratedTrace extends InputTrace

  def insertResetPlaceHolders(program: BrboProgram): BrboProgram = {
    val mainFunctionWithResetPlaceHolders =
      program.mainFunction.replaceBodyWithoutGhostInitialization(
        BrboAstUtils.insertResetPlaceHolder(program.mainFunction.body).asInstanceOf[Statement]
      )
    program.replaceMainFunction(mainFunctionWithResetPlaceHolders)
  }

  def getInputFilePath(useProvidedInputs: Boolean, sourceFilePath: String): Option[String] = {
    if (!useProvidedInputs)
      return None
    assert(FilenameUtils.getExtension(sourceFilePath) == "java")
    val fileName = s"${FilenameUtils.getBaseName(sourceFilePath)}.json"
    Some(s"${FilenameUtils.getFullPath(sourceFilePath)}$fileName")
  }

  def classifierFeatures(program: BrboProgram): List[Identifier] = {
    val loopConditionals = BrboAstUtils.getLoopConditionals(program.mainFunction.body)
    program.mainFunction.nonGhostVariables.filter({
      variable =>
        val usedInLoopConditional = loopConditionals.exists(e => e.getUses.contains(variable))
        val nonGhostVariable = variable.typ match {
          case BrboType.BOOL => true
          case BrboType.INT => !GhostVariableUtils.isGhostVariable(variable.name)
          case BrboType.ARRAY(_) => false
          case _ => false
        }
        nonGhostVariable && usedInLoopConditional
    })
  }
}
