package brbo.common.commandline

import org.kohsuke.args4j.Option

import java.nio.file.Files

class FuzzingArguments extends CommonArguments {
  @Option(name = "--timeout", aliases = Array("-t"), required = false,
    usage = "The timeout for running AFL (in seconds).")
  private var aflTimeoutInSeconds: Int = 30

  @Option(name = "--qfuzz", required = false,
    usage = "The absolute path of QFuzz.")
  protected var qfuzzPath: String = s"${System.getProperty("user.home")}/Documents/workspace/qfuzz_docker/"

  @Option(name = "--output", aliases = Array("-o"), required = false,
    usage = "The directory for storing QFuzz outputs.")
  protected var outputPath: String = Files.createTempDirectory("qfuzz").toAbsolutePath.toString

  @Option(name = "--input", aliases = Array("-i"), required = false,
    usage = "The directory that stores the initial inputs for QFuzz.")
  protected var inputDirectory: String = s"${System.getProperty("user.dir")}/src/main/java/brbo/fuzz/inputs"

  @Option(name = "--dry", required = false,
    usage = "Dry run: Do not write the QFuzz generated inputs into Json files.")
  protected var dryRun: Boolean = false

  @Option(name = "--naive", required = false,
    usage = "Whether to run the naive QFuzz or the modified.")
  protected var naive: Boolean = false

  /**
   * A too large number may result in executing the target program for too long. For example, an array input may
   * contain a large number that controls the loop iterations.
   * A too small number may result in a higher likelihood that the generated inputs do not very much.
   */
  @Option(name = "--max-int", required = false,
    usage = "The max integer for QFuzz to find inputs from.")
  private var maxInteger: Int = 12

  @Option(name = "--min-int", required = false,
    usage = "The min integer for QFuzz to find inputs from.")
  private var minInteger: Int = 4

  def getAflTimeoutInSeconds: Int = aflTimeoutInSeconds

  def getQFuzzPath: String = qfuzzPath

  def getOutputPath: String = outputPath

  def getInputDirectory: String = inputDirectory

  def getDryRun: Boolean = dryRun

  def getNaive: Boolean = naive

  def getMaxInteger: Int = maxInteger

  def getMinInteger: Int = minInteger

  override def toString: String = {
    val strings = List[String]()
    strings.mkString("\n")
  }
}