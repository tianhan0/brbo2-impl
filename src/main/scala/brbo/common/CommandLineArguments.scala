package brbo.common

import brbo.backend.verifier.AmortizationMode._
import brbo.backend.verifier.UAutomizerVerifier
import brbo.common.CommandLineArguments._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommandLineArguments {

  @Option(name = "--amortize", aliases = Array("-a"), required = false,
    usage = "The mode of amortization. Choose from: `FULL`, `NO`, `SELECTIVE`, `ALL` (case-insensitive)")
  private var amortizationMode: String = "selective"

  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  private var debugMode: Boolean = false

  @Option(name = "--print-model-checker-inputs", aliases = Array("-i"), required = false,
    usage = "Print input programs to the model checker.")
  private var printModelCheckerInputs: Boolean = false

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory (which will be recursively traversed) or the file to analyze.")
  private var directoryToAnalyze: String = "."

  @Option(name = "--model-checker-timeout", aliases = Array("-t"),
    usage = "The amount of timeout (unit: seconds) allowed for each invocation to the model checker (i.e., UAutomizer). Negative numbers mean no timeout will be set.")
  private var modelCheckerTimeout: Int = DEFAULT_MODEL_CHECKER_TIME_OUT

  @Option(name = "--print-cfg", aliases = Array("--cfg"), required = false, usage = "Print the control flow graph of the main function (Not implemented!).")
  private var printCFG: Boolean = false

  @Option(name = "--less-precise", required = false, usage = "Verify the bounds specified by function `lessPreciseBound`.")
  private var lessPreciseBound: Boolean = false

  @Option(name = "--max-group", required = false, usage = "The max number of amortization groups")
  private var maxGroups: Int = DEFAULT_MAX_GROUPS

  @Option(name = "--model-checker-path", required = false,
    usage = "The absolute path of the directory containing all files of the model checker.")
  private var modelCheckerDirectory: String = UAutomizerVerifier.TOOL_DIRECTORY

  @Option(name = "--relational-predicates", required = false, usage = "Search relational (i.e., Octagon) predicates when synthesizing new predicates.")
  private var relationalPredicates: Boolean = false

  @Option(name = "--iterations", aliases = Array("--it"), required = false,
    usage = "The max number of refinement iterations when analyzing a program.")
  private var maxIterations: Int = DEFAULT_MAX_ITERATIONS

  def getAmortizationMode: AmortizationMode = {
    amortizationMode.toLowerCase() match {
      case "no" => NO_AMORTIZE
      case "full" => FULL_AMORTIZE
      case "selective" => SELECTIVE_AMORTIZE
      case "all" => ALL_AMORTIZE
      case "unknown" => UNKNOWN_MODE
    }
  }

  def getDebugMode: Boolean = debugMode

  def getDirectoryToAnalyze: String = directoryToAnalyze

  def getPrintModelCheckerInputs: Boolean = printModelCheckerInputs

  def getModelCheckerTimeout: Int = modelCheckerTimeout

  def getPrintCFG: Boolean = printCFG

  def getLessPreciseBound: Boolean = lessPreciseBound

  def getMaxGroups: Int = maxGroups

  def getModelCheckerDirectory: String = modelCheckerDirectory

  def getRelationalPredicates: Boolean = relationalPredicates

  def getMaxIterations: Int = maxIterations

  private var initialized = false

  def initialize(amortizationMode: AmortizationMode,
                 debugMode: Boolean,
                 directoryToAnalyze: String,
                 skipSanityCheck: Boolean,
                 printModelCheckerInputs: Boolean,
                 modelCheckerTimeout: Int,
                 printCFG: Boolean,
                 lessPreciseBound: Boolean,
                 generateSynthetic: Int,
                 maxGroups: Int,
                 modelCheckerDirectory: String,
                 relationalPredicates: Boolean,
                 maxIterations: Int): Unit = {
    if (initialized) {
      logger.info(s"Already initialized")
      return
    }
    initialized = true
    this.amortizationMode = amortizationModeToShortString(amortizationMode)
    this.debugMode = debugMode
    this.directoryToAnalyze = directoryToAnalyze
    this.printModelCheckerInputs = printModelCheckerInputs
    this.modelCheckerTimeout = modelCheckerTimeout
    this.printCFG = printCFG
    this.lessPreciseBound = lessPreciseBound
    this.maxGroups = maxGroups
    this.modelCheckerDirectory = modelCheckerDirectory
    this.relationalPredicates = relationalPredicates
    this.maxIterations = maxIterations
  }

  override def toString: String = {
    val strings = List[String](
      s"Infer resource usage upper bounds for each method in each file `*.java` under directory `$directoryToAnalyze`",
      s"Amortization mode: `$getAmortizationMode`",
      s"Debug mode? `$debugMode`",
      s"Print inputs to the model checker? `$printModelCheckerInputs`",
      s"Model checker's time out: `$modelCheckerTimeout` seconds",
      s"Print CFG? `$printCFG`",
      s"Check less precise bounds? `$lessPreciseBound`",
      s"Max number of groups: `$maxGroups`",
      s"Model check's directory is `$modelCheckerDirectory`",
      s"Search relational predicates? `$relationalPredicates`",
      s"Max number of refinement iterations: `$maxIterations`",
    )
    strings.mkString("\n")
  }

  def toFileName: String = {
    val amortizationMode = amortizationModeToShortString(getAmortizationMode)
    val timeout = if (modelCheckerTimeout < 0) "noTimeout" else s"${modelCheckerTimeout}s"
    val boundPrecision = s"${if (lessPreciseBound) "less" else "most"}Precise"
    s"""$amortizationMode-$timeout-$boundPrecision"""
  }
}

object CommandLineArguments {
  val DEFAULT_MAX_GROUPS = 3
  val DEFAULT_TIMEOUT = 20
  val DEFAULT_MAX_ITERATIONS = 1000

  private val logger = LogManager.getLogger(CommandLineArguments.getClass.getName)

  def parseArguments(args: Array[String]): CommandLineArguments = {
    val arguments = new CommandLineArguments
    val parser = new CmdLineParser(arguments)
    try {
      parser.parseArgument(args.toList.asJava)
    } catch {
      case e: CmdLineException =>
        logger.fatal(s"Error:${e.getMessage}\nUsage:\n")
        parser.printUsage(System.out)
        System.exit(1)
    }
    arguments
  }

  val DEFAULT_MODEL_CHECKER_TIME_OUT = 60 // Unit: Second

  val DEFAULT_ARGUMENTS: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(
      UNKNOWN_MODE,
      debugMode = false,
      "",
      skipSanityCheck = false,
      printModelCheckerInputs = false,
      modelCheckerTimeout = DEFAULT_TIMEOUT,
      printCFG = false,
      lessPreciseBound = false,
      generateSynthetic = 0,
      maxGroups = DEFAULT_MAX_GROUPS,
      modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false,
      maxIterations = DEFAULT_MAX_ITERATIONS,
    )
    arguments
  }

  val DEBUG_MODE_ARGUMENTS: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(
      UNKNOWN_MODE,
      debugMode = true,
      "",
      skipSanityCheck = false,
      printModelCheckerInputs = false,
      modelCheckerTimeout = DEFAULT_TIMEOUT,
      printCFG = false,
      lessPreciseBound = false,
      generateSynthetic = 0,
      maxGroups = DEFAULT_MAX_GROUPS,
      modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false,
      maxIterations = DEFAULT_MAX_ITERATIONS,
    )
    arguments
  }
}