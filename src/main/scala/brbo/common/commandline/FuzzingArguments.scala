package brbo.common.commandline

import org.kohsuke.args4j.Option

class FuzzingArguments extends CommonArguments {
  @Option(name = "--timeout", aliases = Array("-t"),
    usage = "The timeout for running AFL (in seconds).")
  private var aflTimeoutInSeconds: Int = 30

  @Option(name = "--qfuzz", required = false,
    usage = "The absolute path of QFuzz.")
  protected var qfuzzPath: String = s"${System.getProperty("user.home")}/Documents/workspace/qfuzz"

  def getAflTimeoutInSeconds: Int = aflTimeoutInSeconds

  def getQFuzzPath: String = qfuzzPath

  override def toString: String = {
    val strings = List[String]()
    strings.mkString("\n")
  }
}