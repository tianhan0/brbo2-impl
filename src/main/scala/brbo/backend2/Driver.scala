package brbo.backend2

import brbo.BrboMain.readFromFile
import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.Trace
import brbo.backend2.learning.SegmentClustering.printSegments
import brbo.backend2.learning.{Classifier, SegmentClustering, TracePartition}
import brbo.common.ast._
import brbo.common.cfg.ControlFlowGraph
import brbo.common.{BrboType, GhostVariableUtils, MyLogger, NewCommandLineArguments}
import org.apache.commons.io.FilenameUtils

import java.io.File

class Driver(arguments: NewCommandLineArguments, program: BrboProgram, inputFilePath: Option[String]) {
  private val debugMode = arguments.getDebugMode
  private val logger = MyLogger.createLogger(classOf[Driver], debugMode)
  logger.info(s"Step 0: Insert reset place holders")
  private val instrumentedProgram = Driver.insertResetPlaceHolders(program)
  private val interpreter = new Interpreter(instrumentedProgram, debugMode)
  private val features = Driver.classifierFeatures(instrumentedProgram)
  logger.info(s"Step 0: Find classifier features {${features.map(i => i.printToIR()).mkString(", ")}}")
  private val segmentClustering = new SegmentClustering(
    sumWeight = 1,
    commandWeight = 0,
    debugMode = debugMode,
    algorithm = arguments.getAlgorithm,
    threads = arguments.getThreads,
  )
  private var numberOfTraces = 0

  def decompose(): BrboProgram = {
    logger.info(s"Step 1: Generate traces")
    logger.info(s"Step 1.1: User-provided inputs")
    val userProvidedTraces: List[Trace] = {
      if (arguments.getUseProvidedInputs && inputFilePath.isDefined) {
        val inputFileContents = readFromFile(inputFilePath.get)
        val inputs = InputParser.parse(inputFileContents)
        Interpreter.generateTraces(inputs = inputs, interpreter = interpreter,
          threads = arguments.getThreads, debugMode = debugMode)
      } else Nil
    }
    logger.info(s"Step 1.2: Fuzzer generated inputs")
    val fuzzerGeneratedTraces: List[Trace] = {
      Fuzzer.fuzz(brboProgram = instrumentedProgram, interpreter = interpreter, debugMode = debugMode,
        samples = arguments.getFuzzSamples, threads = arguments.getThreads)
    }
    numberOfTraces = userProvidedTraces.size + fuzzerGeneratedTraces.size
    logger.info(s"Step 2: Select representative traces")
    val representatives = TracePartition.selectRepresentatives(
      userProvidedTraces = userProvidedTraces,
      fuzzerGeneratedTraces = fuzzerGeneratedTraces)
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
        trace.toTable(printStores = true, omitExpressions = true, onlyGhostCommand = false)._1.printAll()
      else
        trace.toTable(printStores = false, omitExpressions = true, onlyGhostCommand = true)._1.printAll()
    logger.info(s"Step 3.1: Trace (length ${trace.length}):\n$representativeTraceString")
    val decomposition = segmentClustering.decompose(
      trace = trace,
      similarTraces = similarTraces,
      interpreter = interpreter,
      features = features
    )
    val groups = decomposition.getGroups
    val groupsString = groups.map({
      case (groupID, group) => s"$groupID: ${printSegments(group.segments)}"
    }).mkString("\n")
    logger.info(s"Step 3.1: Selected decomposition:\n$groupsString\n${SegmentClustering.printDecomposition(trace, groups)}")
    logger.info(s"Step 3.2: Generate tables for training classifiers")
    val tables = Classifier.generateTables(
      trace = trace,
      evaluate = Classifier.evaluateFromInterpreter(interpreter),
      groups = groups,
      features = features,
      throwIfNoResetPlaceHolder = false,
      controlFlowGraph = ControlFlowGraph.toControlFlowGraph(instrumentedProgram)
    )
    // logger.traceOrError(s"Step 3.2: Generated tables:\n${tables.print()}")
    logger.info(s"Step 3.3: Generate classifiers on the tables")
    val classifierResults = tables.toProgramTables.generateClassifiers(debugMode)
    logger.info(s"Step 3.4: Generate program transformations")
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

  def getInputFilePath(useProvidedInputs: Boolean, javaFile: File): Option[String] = {
    if (!useProvidedInputs)
      return None
    val absolutePath = javaFile.getAbsolutePath
    assert(FilenameUtils.getExtension(absolutePath) == "java")
    val testFileName = s"${FilenameUtils.getBaseName(absolutePath)}.json"
    Some(s"${FilenameUtils.getFullPath(absolutePath)}$testFileName")
  }

  def classifierFeatures(program: BrboProgram): List[Identifier] = {
    val loopConditionals = BrboAstUtils.getLoopConditionals(program.mainFunction.body)
    program.mainFunction.nonGhostVariables().filter({
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
