package brbo.common.commandline

import brbo.common.commandline.CommonArguments._
import org.apache.logging.log4j.LogManager
import org.kohsuke.args4j.{CmdLineException, CmdLineParser, Option}

import scala.collection.JavaConverters._

class CommonArguments {
  @Option(name = "--debug", aliases = Array("-g"), required = false,
    usage = "Turn on the debug mode.")
  protected var debugMode: Boolean = DEFAULT_DEBUG_MODE

  @Option(name = "--directory", aliases = Array("-d"), required = true,
    usage = "The directory (which will be " + "recursively traversed) or the file to analyze.")
  protected var directoryToAnalyze: String = DEFAULT_DIRECTORY_TO_ANALYZE

  def getDebugMode: Boolean = debugMode

  def getDirectoryToAnalyze: String = directoryToAnalyze

  private var initialized = false

  def initialize(debugMode: Boolean, directoryToAnalyze: String): Unit = {
    if (initialized) throw new Exception(s"Already initialized")
    initialized = true
    this.debugMode = debugMode
    this.directoryToAnalyze = directoryToAnalyze
  }
}

object CommonArguments {
  val DEFAULT_DIRECTORY_TO_ANALYZE = "."
  val DEFAULT_DEBUG_MODE = false

  private val logger = LogManager.getLogger(CommonArguments.getClass.getName)

  def parseArguments(args: Array[String], subcommand: Subcommand.AbstractSubcommand): CommonArguments = {
    val arguments: CommonArguments = {
      subcommand match {
        case Subcommand.Decompose => new DecompositionArguments
        case Subcommand.Fuzz => new FuzzingArguments
        case Subcommand.TransformToBrbo => new TransformToBrboArguments
        case _ => throw new Exception
      }
    }
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