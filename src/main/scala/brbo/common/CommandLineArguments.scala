package brbo.common

import brbo.backend.verifier.AmortizationMode._
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.modelchecker.AbstractDomainName
import brbo.backend.verifier.modelchecker.AbstractDomainName.{AbstractDomainName, OCTAGON, POLKA_NONSTRICT, POLKA_STRICT}
import brbo.common.CommandLineArguments._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommandLineArguments {

  @Option(name = "--amortize", aliases = Array("-a"), required = false,
    usage = "The amortization mode. Choose from: `FULL`, `NO`, `SELECTIVE`, `ALL` (case-insensitive)")
  private var amortizationMode: String = "selective"

  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  private var debugMode: Boolean = false

  @Option(name = "--print-verifier-inputs", aliases = Array("-i"), required = false,
    usage = "Print input programs to the verifier.")
  private var printVerifierInputs: Boolean = false

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory (which will be " + "recursively traversed) or the file to analyze.")
  private var directoryToAnalyze: String = "."

  @Option(name = "--verifier-timeout", aliases = Array("-t"),
    usage = "The amount of timeout (unit: seconds) allowed for each invocation to the verifier. Negative numbers mean no timeout.")
  private var verifierTimeout: Int = DEFAULT_VERIFIER_TIME_OUT

  @Option(name = "--print-cfg", aliases = Array("--cfg"), required = false,
    usage = "Print the control flow graph of the main function.")
  private var printCFG: Boolean = false

  @Option(name = "--max-group", required = false,
    usage = "The max number of amortization groups.")
  private var maxGroups: Int = DEFAULT_MAX_GROUPS

  @Option(name = "--verifier-path", required = false,
    usage = "The absolute path of the directory containing all files of the verifier.")
  private var verifierDirectory: String = UAutomizerVerifier.TOOL_DIRECTORY

  @Option(name = "--relational-predicates", required = false,
    usage = "Search relational (i.e., Octagon) predicates " + "when synthesizing new predicates.")
  private var relationalPredicates: Boolean = false

  @Option(name = "--iterations", aliases = Array("--it"), required = false,
    usage = "The max number of refinement iterations when analyzing a program.")
  private var maxIterations: Int = DEFAULT_MAX_ITERATIONS

  @Option(name = "--assertion-index", aliases = Array("--idx"), required = false,
    usage = "Only verify the assertion associated with this tag. Verify all assertions if the tag is the default value.")
  private var assertionTag: String = DEFAULT_ASSERTION_TAG

  @Option(name = "--abstract-domain", aliases = Array("--dom"), required = false,
    usage = "The abstract domain to use in the verifier. Choose from: `OCTAGON`, `POLKA_STRICT`, `POLKA_NONSTRICT` (case-insensitive)")
  private var abstractDomain: String = DEFAULT_ABSTRACT_DOMAIN

  @Option(name = "--max-path-length", aliases = Array("--len"), required = false,
    usage = "The maximum length of a single path that the verifier will explore.")
  private var maxPathLength: Int = DEFAULT_MAX_PATH_LENGTH

  @Option(name = "--use-z3", aliases = Array("--z3"), required = false,
    usage = "Check the satisfiability of abstract states (represented in Apron) with Z3, instead of using Apron.")
  private var checkWithZ3: Boolean = false

  def getAmortizationMode: AmortizationMode = {
    amortizationMode.toLowerCase() match {
      case "no" => NO_AMORTIZE
      case "full" => FULL_AMORTIZE
      case "selective" => SELECTIVE_AMORTIZE
      case "all" => ALL_AMORTIZE
      case "unknown" => TEST_MODE
    }
  }

  def getDebugMode: Boolean = debugMode

  def getDirectoryToAnalyze: String = directoryToAnalyze

  def getPrintVerifierInputs: Boolean = printVerifierInputs

  def getVerifierTimeout: Int = verifierTimeout

  def getPrintCFG: Boolean = printCFG

  def getMaxGroups: Int = maxGroups

  def getVerifierDirectory: String = verifierDirectory

  def getRelationalPredicates: Boolean = relationalPredicates

  def getMaxIterations: Int = maxIterations

  def getAssertionIndex: String = assertionTag

  def getAbstractDomain: AbstractDomainName = {
    val upperCase = abstractDomain.toUpperCase()
    if (upperCase == OCTAGON.toString) OCTAGON
    else if (upperCase == POLKA_STRICT.toString) POLKA_STRICT
    else if (upperCase == POLKA_NONSTRICT.toString) POLKA_NONSTRICT
    else throw new Exception
  }

  def getMaxPathLength: Int = maxPathLength

  def getCheckWithZ3: Boolean = checkWithZ3

  private var initialized = false

  def initialize(amortizationMode: AmortizationMode, debugMode: Boolean, directoryToAnalyze: String,
                 printVerifierInputs: Boolean, verifierTimeout: Int,
                 printCFG: Boolean, generateSynthetic: Int, maxGroups: Int, verifierDirectory: String,
                 relationalPredicates: Boolean, maxIterations: Int, assertionTag: String,
                 abstractDomain: String, maxPathLength: Int, checkWithZ3: Boolean): Unit = {
    if (initialized) throw new Exception(s"Already initialized")
    initialized = true
    this.amortizationMode = amortizationModeToShortString(amortizationMode)
    this.debugMode = debugMode
    this.directoryToAnalyze = directoryToAnalyze
    this.printVerifierInputs = printVerifierInputs
    this.verifierTimeout = verifierTimeout
    this.printCFG = printCFG
    this.maxGroups = maxGroups
    this.verifierDirectory = verifierDirectory
    this.relationalPredicates = relationalPredicates
    this.maxIterations = maxIterations
    this.assertionTag = assertionTag
    this.abstractDomain = abstractDomain
    this.maxPathLength = maxPathLength
    this.checkWithZ3 = checkWithZ3
  }

  override def toString: String = {
    val strings = List[String](
      s"Verify resource usage assertions for each `*.java` file under directory `$directoryToAnalyze`",
      s"Amortization mode: `$getAmortizationMode`",
      s"Debug mode? `$debugMode`",
      s"Print inputs to the verifier? `$printVerifierInputs`",
      s"Verifier time out: `$verifierTimeout` seconds",
      s"Print CFG? `$printCFG`",
      s"Max number of groups: `$maxGroups`",
      s"Verifier directory: `$verifierDirectory`",
      s"Search relational predicates? `$relationalPredicates`",
      s"Max number of refinement iterations: `$maxIterations`",
      s"The index of the assertion to verify: `$assertionTag`",
      s"The abstract domain to use: `$getAbstractDomain`",
      s"The maximum length of a single path that the verifier will explore: `$maxPathLength`",
      s"Check the satisfiability of abstract states with Z3? `$checkWithZ3`"
    )
    strings.mkString("\n")
  }

  def toFileName: String = {
    val amortizationMode = amortizationModeToShortString(getAmortizationMode)
    val timeoutString = if (verifierTimeout < 0) "noTimeout" else s"${verifierTimeout}s"
    val assertionIndexString = s"assert-$assertionTag"
    val checkWithZ3String = if (checkWithZ3) "z3" else ""
    s"""$amortizationMode-$timeoutString-$assertionIndexString-$abstractDomain-$checkWithZ3String"""
  }
}

object CommandLineArguments {
  val DEFAULT_MAX_GROUPS = 3
  val DEFAULT_TIMEOUT = 20
  val DEFAULT_MAX_ITERATIONS = 1000
  val DEFAULT_ASSERTION_TAG: String = "all"
  val DEFAULT_ABSTRACT_DOMAIN: String = OCTAGON.toString
  val DEFAULT_MAX_PATH_LENGTH = 100

  private val logger = LogManager.getLogger(CommandLineArguments.getClass.getName)

  def parseArguments(args: Array[String]): CommandLineArguments = {
    val arguments = new CommandLineArguments
    val parser = new CmdLineParser(arguments)
    try {
      parser.parseArgument(args.toList.asJava)
    } catch {
      case e: CmdLineException => logger.fatal(s"Error:${e.getMessage}\nUsage:\n")
        parser.printUsage(System.out)
        System.exit(1)
    }
    arguments
  }

  val DEFAULT_VERIFIER_TIME_OUT = 60 // Unit: Second

  val DEFAULT_ARGUMENTS: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode = false, "",
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      generateSynthetic = 0, maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, maxIterations = DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = DEFAULT_ABSTRACT_DOMAIN, maxPathLength = DEFAULT_MAX_PATH_LENGTH, checkWithZ3 = false)
    arguments
  }

  val DEBUG_MODE_ARGUMENTS: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode = true, "",
      printVerifierInputs = false, verifierTimeout = DEFAULT_TIMEOUT, printCFG = false,
      generateSynthetic = 0, maxGroups = DEFAULT_MAX_GROUPS,
      verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = false, maxIterations = DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = DEFAULT_ABSTRACT_DOMAIN, maxPathLength = DEFAULT_MAX_PATH_LENGTH, checkWithZ3 = false)
    arguments
  }
}