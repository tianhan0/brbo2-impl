package brbo.common

import brbo.backend.verifier.AmortizationMode._
import brbo.backend.verifier.UAutomizerVerifier
import brbo.common.CommandLineArguments.logger
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

  @Option(name = "--skip-sanity-check", aliases = Array("-s"), required = false,
    usage = "Skip the sanity check.")
  private var skipSanityCheck: Boolean = false

  @Option(name = "--print-model-checker-inputs", aliases = Array("-i"), required = false,
    usage = "Print input programs to the model checker.")
  private var printModelCheckerInputs: Boolean = false

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory (which will be recursively traversed) or the file to analyze.")
  private var directoryToAnalyze: String = "."

  @Option(name = "--model-checker-timeout", aliases = Array("-t"),
    usage = "The amount of timeout (unit: seconds) allowed for each invocation to the model checker (i.e., UAutomizer). Negative numbers mean no timeout will be set.")
  private var modelCheckerTimeout: Int = CommandLineArguments.DEFAULT_MODEL_CHECKER_TIME_OUT

  @Option(name = "--print-cfg", aliases = Array("--cfg"), required = false, usage = "Print the control flow graph of the input graph.")
  private var printCFG: Boolean = false

  @Option(name = "--less-precise", required = false, usage = "Verify the bounds specified by function `lessPreciseBound`.")
  private var lessPreciseBound: Boolean = false

  @Option(name = "--generate-synthetic", required = false, usage = "Generate `n` synthetic programs")
  private var generateSynthetic: Int = 0

  @Option(name = "--max-group", required = false, usage = "The max number of amortization groups")
  private var maxGroups: Int = CommandLineArguments.DEFAULT_MAX_GROUPS

  @Option(name = "--model-checker-path", required = false,
    usage = "The absolute path of the directory containing all files of the model checker.")
  private var modelCheckerDirectory: String = UAutomizerVerifier.TOOL_DIRECTORY

  @Option(name = "--relational-predicates", required = false, usage = "Search relational (i.e., Octagon) predicates when synthesizing new predicates.")
  private var relationalPredicates: Boolean = false

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

  def getSkipSanityCheck: Boolean = skipSanityCheck

  def getPrintModelCheckerInputs: Boolean = printModelCheckerInputs

  def getModelCheckerTimeout: Int = modelCheckerTimeout

  def getPrintCFG: Boolean = printCFG

  def getLessPreciseBound: Boolean = lessPreciseBound

  def getGenerateSynthetic: Int = generateSynthetic

  def getMaxGroups: Int = maxGroups

  def getModelCheckerDirectory: String = modelCheckerDirectory

  def getRelationalPredicates: Boolean = relationalPredicates

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
                 relationalPredicates: Boolean): Unit = {
    if (initialized) {
      logger.info(s"Already initialized")
      return
    }
    initialized = true
    this.amortizationMode = amortizationModeToShortString(amortizationMode)
    this.debugMode = debugMode
    this.directoryToAnalyze = directoryToAnalyze
    this.skipSanityCheck = skipSanityCheck
    this.printModelCheckerInputs = printModelCheckerInputs
    this.modelCheckerTimeout = modelCheckerTimeout
    this.printCFG = printCFG
    this.lessPreciseBound = lessPreciseBound
    this.generateSynthetic = generateSynthetic
    this.maxGroups = maxGroups
    this.modelCheckerDirectory = modelCheckerDirectory
    this.relationalPredicates = relationalPredicates
  }

  override def toString: String = {
    val strings = List[String](
      s"Infer resource usage upper bounds for each method in each file `*.java` under directory `$directoryToAnalyze`",
      s"Amortization mode: `$getAmortizationMode`",
      s"Debug mode? `$debugMode`",
      s"Skip sanity check? `$skipSanityCheck`",
      s"Print inputs to the model checker? `$printModelCheckerInputs`",
      s"Model checker's time out: `$modelCheckerTimeout` seconds",
      s"Print CFG? `$printCFG`",
      s"Check less precise bounds? `$lessPreciseBound`",
      s"Generate `$generateSynthetic` synthetic programs",
      s"Max number of groups: `$maxGroups`",
      s"Model check's directory is `$modelCheckerDirectory`",
      s"Search relational predicates? `$relationalPredicates`",
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
  private val DEFAULT_MAX_GROUPS = 3

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
      modelCheckerTimeout = 20,
      printCFG = false,
      lessPreciseBound = false,
      generateSynthetic = 0,
      maxGroups = DEFAULT_MAX_GROUPS,
      modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false,
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
      modelCheckerTimeout = 20,
      printCFG = false,
      lessPreciseBound = false,
      generateSynthetic = 0,
      maxGroups = DEFAULT_MAX_GROUPS,
      modelCheckerDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false,
    )
    arguments
  }
}