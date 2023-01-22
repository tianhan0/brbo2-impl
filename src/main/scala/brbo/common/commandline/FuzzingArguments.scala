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
  protected var inputPath: String = s"${System.getProperty("user.dir")}/src/main/java/brbo/fuzz/inputs"

  @Option(name = "--dry", required = false,
    usage = "Dry run: Do not write the QFuzz generated inputs into Json files.")
  protected var dryRun: Boolean = false

  @Option(name = "--naive", required = false,
    usage = "Whether to run the naive QFuzz or the modified.")
  protected var naive: Boolean = false

  def getAflTimeoutInSeconds: Int = aflTimeoutInSeconds

  def getQFuzzPath: String = qfuzzPath

  def getOutputPath: String = outputPath

  def getInputPath: String = inputPath

  def getDryRun: Boolean = dryRun

  def getNaive: Boolean = naive

  override def toString: String = {
    val strings = List[String]()
    strings.mkString("\n")
  }
}