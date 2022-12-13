package brbo.common

import brbo.backend2.Fuzzer
import brbo.backend2.learning.ScriptRunner.{Algorithm, Euclidean, KMeans, Optics}
import brbo.backend2.learning.SegmentClustering
import brbo.common.CommandLineArguments._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class NewCommandLineArguments extends Serializable {
  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  private var debugMode: Boolean = DEFAULT_DEBUG_MODE

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory (which will be " + "recursively traversed) or the file to analyze.")
  private var directoryToAnalyze: String = DEFAULT_DIRECTORY_TO_ANALYZE

  @Option(name = "--samples", aliases = Array("-s"),
    usage = "The number of samples to fuzz, for every integer-typed input.")
  private var fuzzSamples: Int = Fuzzer.DEFAULT_SAMPLES

  @Option(name = "--algorithm", aliases = Array("-a"),
    usage = "The cluster algorithm to use. Choose from `optics` or `kmeans`.")
  private var algorithm: String = "optics"

  @Option(name = "--parameter", aliases = Array("-p"),
    usage = "The parameter of the algorithm. For `optics`, the parameter is `max-eps`. For `kmeans`, the parameter is the number of clusters.")
  private var algorithmParameter: Double = -1

  @Option(name = "--threads", aliases = Array("-t"),
    usage = "The number of threads when computing in parallel.")
  private var threads: Int = SegmentClustering.THREADS

  @Option(name = "--input", aliases = Array("-i"), required = false,
    usage = "Use the provided inputs. For java program a/b/Test.java, its input file is a/b/Test.input.")
  private var useProvidedInputs: Boolean = false

  def getDebugMode: Boolean = debugMode

  def getDirectoryToAnalyze: String = directoryToAnalyze

  def getFuzzSamples: Int = fuzzSamples

  def getThreads: Int = threads

  def getAlgorithm: Algorithm = {
    val parameter = if (algorithmParameter < 0) None else Some(algorithmParameter)
    algorithm match {
      case "optics" =>
        Optics(maxEps = parameter, Euclidean)
      case "kmeans" => KMeans(clusters = parameter.map(d => d.toInt))
    }
  }

  def getUseProvidedInputs: Boolean = useProvidedInputs

  private var initialized = false

  def initialize(debugMode: Boolean, directoryToAnalyze: String): Unit = {
    if (initialized) throw new Exception(s"Already initialized")
    initialized = true
    this.debugMode = debugMode
    this.directoryToAnalyze = directoryToAnalyze
  }

  override def toString: String = {
    val strings = List[String](
      s"fuzzSamples: $fuzzSamples",
      s"algorithm: $algorithm",
      s"algorithmParameter: $algorithmParameter",
      s"threads: $threads",
      s"userProvidedInputs: $useProvidedInputs"
    )
    strings.mkString("\n")
  }
}

object NewCommandLineArguments {
  val DEFAULT_DIRECTORY_TO_ANALYZE = "."
  val DEFAULT_DEBUG_MODE = false

  private val logger = LogManager.getLogger(CommandLineArguments.getClass.getName)

  def parseArguments(args: Array[String]): NewCommandLineArguments = {
    val arguments = new NewCommandLineArguments
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
}