package brbo.common.commandline

import org.kohsuke.args4j.Option

import java.nio.file.Files

class FuzzingArguments extends CommonArguments {
  @Option(name = "--timeout", aliases = Array("-t"), required = false,
    usage = "The timeout for running AFL (in seconds).")
  private var aflTimeoutInSeconds: Int = 30

  @Option(name = "--qfuzz", required = false,
    usage = "The absolute path of QFuzz.")
  protected var qfuzzPath: String = s"${System.getProperty("user.home")}/Documents/workspace/qfuzz"

  @Option(name = "--output", aliases = Array("-o"), required = false,
    usage = "The directory for storing QFuzz outputs.")
  protected var outputPath: String = Files.createTempDirectory("qfuzz").toAbsolutePath.toString

  def getAflTimeoutInSeconds: Int = aflTimeoutInSeconds

  def getQFuzzPath: String = qfuzzPath

  def getOutputPath: String = outputPath

  override def toString: String = {
    val strings = List[String]()
    strings.mkString("\n")
  }
}