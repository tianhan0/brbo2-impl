package brbo.common

import brbo.backend.verifier.AmortizationMode._
import brbo.backend.verifier.UAutomizerVerifier
import brbo.backend.verifier.modelchecker.AbstractDomainName.{AbstractDomainName, OCTAGON, POLKA_NONSTRICT, POLKA_STRICT}
import brbo.common.CommandLineArguments._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommandLineArguments extends Serializable {

  @Option(name = "--amortize", aliases = Array("-a"), required = false,
    usage = "The amortization mode. Choose from: `FULL`, `NO`, `SELECTIVE`, `ALL` (case-insensitive)")
  private var amortizationMode: String = "selective"

  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  private var debugMode: Boolean = DEFAULT_DEBUG_MODE

  @Option(name = "--print-verifier-inputs", aliases = Array("-i"), required = false,
    usage = "Print input programs to the verifier.")
  private var printVerifierInputs: Boolean = DEFAULT_PRINT_VERIFIER_INPUTS

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory (which will be " + "recursively traversed) or the file to analyze.")
  private var directoryToAnalyze: String = DEFAULT_DIRECTORY_TO_ANALYZE

  @Option(name = "--verifier-timeout", aliases = Array("-t"),
    usage = "The amount of timeout (unit: seconds) allowed for each invocation to the verifier. Negative numbers mean no timeout.")
  private var verifierTimeout: Int = DEFAULT_VERIFIER_TIME_OUT

  @Option(name = "--print-cfg", aliases = Array("--cfg"), required = false,
    usage = "Print the control flow graph of the main function.")
  private var printCFG: Boolean = DEFAULT_PRINT_CFG

  @Option(name = "--max-group", required = false,
    usage = "The max number of amortization groups.")
  private var maxGroups: Int = DEFAULT_MAX_GROUPS

  @Option(name = "--verifier-path", required = false,
    usage = "The absolute path of the directory containing all files of the verifier.")
  private var verifierDirectory: String = UAutomizerVerifier.TOOL_DIRECTORY

  @Option(name = "--relational-predicates", required = false,
    usage = "Search relational (i.e., Octagon) predicates " + "when synthesizing new predicates.")
  private var relationalPredicates: Boolean = DEFAULT_RELATIONAL_PREDICATES

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
  private var checkWithZ3: Boolean = DEFAULT_CHECK_WITH_Z3

  @Option(name = "--positive-inputs", required = false,
    usage = "Assume inputs are all positive numbers. Such information is used to recover imprecision caused by widening.")
  private var assumePositiveInputs: Boolean = DEFAULT_ASSUME_POSITIVE_INPUTS

  @Option(name = "--widen", aliases = Array("-w"), required = false,
    usage = "The number of visits to a same location before widening all abstracts at this location.")
  private var widenThreshold: Int = DEFAULT_WIDEN_THRESHOLD

  @Option(name = "--threads", required = false,
    usage = "The number of threads to use when finding path refinements.")
  private var numberOfThreads: Int = DEFAULT_NUMBER_OF_THREADS

  def getAmortizationMode: AmortizationMode = {
    amortizationMode.toLowerCase() match {
      case "no" => NO_AMORTIZE
      case "full" => FULL_AMORTIZE
      case "selective" => SELECTIVE_AMORTIZE
      case "all" => ALL_AMORTIZE
      case "test" => TEST_MODE
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

  def getAssumePositiveInputs: Boolean = assumePositiveInputs

  def getWidenThreshold: Int = widenThreshold

  def getThreads: Int = numberOfThreads

  private var initialized = false

  def initialize(amortizationMode: AmortizationMode, debugMode: Boolean, directoryToAnalyze: String,
                 printVerifierInputs: Boolean, verifierTimeout: Int,
                 printCFG: Boolean, maxGroups: Int, verifierDirectory: String,
                 relationalPredicates: Boolean, maxIterations: Int, assertionTag: String,
                 abstractDomain: String, maxPathLength: Int, checkWithZ3: Boolean, assumePositiveInputs: Boolean,
                 widenThreshold: Int, numberOfThreads: Int): Unit = {
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
    this.assumePositiveInputs = assumePositiveInputs
    this.widenThreshold = widenThreshold
    this.numberOfThreads = numberOfThreads
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
      s"Check the satisfiability of abstract states with Z3? `$checkWithZ3`",
      s"Assume all inputs are positive? `$assumePositiveInputs`",
      s"Widen threshold: `$widenThreshold`",
      s"Number of threads: `$numberOfThreads`",
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

  def copyNoWidening(): CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(getAmortizationMode, debugMode, directoryToAnalyze, printVerifierInputs,
      verifierTimeout, printCFG, maxGroups, verifierDirectory, relationalPredicates, maxIterations,
      assertionTag, abstractDomain, maxPathLength, checkWithZ3, assumePositiveInputs, widenThreshold = Int.MaxValue,
      numberOfThreads)
    arguments
  }
}

object CommandLineArguments {
  val DEFAULT_DIRECTORY_TO_ANALYZE = "."
  val DEFAULT_DEBUG_MODE = false
  val DEFAULT_PRINT_CFG = false
  val DEFAULT_PRINT_VERIFIER_INPUTS = false
  val DEFAULT_RELATIONAL_PREDICATES = false
  val DEFAULT_CHECK_WITH_Z3 = true
  val DEFAULT_ASSUME_POSITIVE_INPUTS = true
  val DEFAULT_MAX_GROUPS = 3
  val DEFAULT_TIMEOUT = 20
  val DEFAULT_MAX_ITERATIONS = 1000
  val DEFAULT_ASSERTION_TAG: String = "all"
  val DEFAULT_ABSTRACT_DOMAIN: String = POLKA_STRICT.toString
  val DEFAULT_MAX_PATH_LENGTH = 30
  val DEFAULT_VERIFIER_TIME_OUT = 60 // Unit: Second
  val DEFAULT_WIDEN_THRESHOLD = 4
  val DEFAULT_NUMBER_OF_THREADS: Int = -1

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

  val TEST_ARGUMENTS: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode = DEFAULT_DEBUG_MODE, directoryToAnalyze = DEFAULT_DIRECTORY_TO_ANALYZE,
      printVerifierInputs = DEFAULT_PRINT_VERIFIER_INPUTS, verifierTimeout = DEFAULT_TIMEOUT, printCFG = DEFAULT_PRINT_CFG,
      maxGroups = DEFAULT_MAX_GROUPS, verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = DEFAULT_RELATIONAL_PREDICATES, maxIterations = DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = DEFAULT_ABSTRACT_DOMAIN, maxPathLength = DEFAULT_MAX_PATH_LENGTH,
      checkWithZ3 = DEFAULT_CHECK_WITH_Z3, assumePositiveInputs = DEFAULT_ASSUME_POSITIVE_INPUTS, widenThreshold = DEFAULT_WIDEN_THRESHOLD,
      numberOfThreads = DEFAULT_NUMBER_OF_THREADS)
    arguments
  }

  val TEST_ARGUMENTS_DEBUG_MODE: CommandLineArguments = {
    val arguments = new CommandLineArguments
    arguments.initialize(TEST_MODE, debugMode = !DEFAULT_DEBUG_MODE, directoryToAnalyze = DEFAULT_DIRECTORY_TO_ANALYZE,
      printVerifierInputs = DEFAULT_PRINT_VERIFIER_INPUTS, verifierTimeout = DEFAULT_TIMEOUT, printCFG = DEFAULT_PRINT_CFG,
      maxGroups = DEFAULT_MAX_GROUPS, verifierDirectory = UAutomizerVerifier.TOOL_DIRECTORY,
      relationalPredicates = DEFAULT_RELATIONAL_PREDICATES, maxIterations = DEFAULT_MAX_ITERATIONS, assertionTag = DEFAULT_ASSERTION_TAG,
      abstractDomain = DEFAULT_ABSTRACT_DOMAIN, maxPathLength = DEFAULT_MAX_PATH_LENGTH,
      checkWithZ3 = DEFAULT_CHECK_WITH_Z3, assumePositiveInputs = DEFAULT_ASSUME_POSITIVE_INPUTS, widenThreshold = DEFAULT_WIDEN_THRESHOLD,
      numberOfThreads = DEFAULT_NUMBER_OF_THREADS)
    arguments
  }
}