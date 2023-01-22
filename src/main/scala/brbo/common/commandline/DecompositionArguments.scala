package brbo.common.commandline

import brbo.backend2.Fuzzer
import brbo.backend2.learning.ScriptRunner.{Algorithm, Euclidean, KMeans, Optics}
import brbo.backend2.learning.SegmentClustering
import org.kohsuke.args4j.Option

class DecompositionArguments extends CommonArguments {
  @Option(name = "--samples", aliases = Array("-s"),
    usage = "The number of samples to fuzz, for every integer-typed input.")
  private var fuzzSamples: Int = Fuzzer.DEFAULT_SAMPLES

  @Option(name = "--algorithm", aliases = Array("-a"),
    usage = "The cluster algorithm to use. Choose from `optics` or `kmeans`.")
  private var algorithm: String = "optics"

  @Option(name = "--parameter", aliases = Array("-p"),
    usage = "The parameter of the clustering algorithm. For `optics`, the parameter is `max-eps`. For `kmeans`, the parameter is the number of clusters.")
  private var algorithmParameter: Double = -1

  @Option(name = "--threads", aliases = Array("-t"),
    usage = "The number of threads when computing in parallel.")
  private var threads: Int = SegmentClustering.THREADS

  @Option(name = "--input", aliases = Array("-i"), required = false,
    usage = "Use the provided inputs. For java program /a/b/Test.java, its input file is /a/b/Test.json.")
  private var useProvidedInputs: Boolean = false

  @Option(name = "--must-reset", required = false,
    usage = "Fail the decomposition if the required resets cannot be inserted.")
  private var throwIfNoResetPlaceHolder: Boolean = false

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

  def getThrowIfNoResetPlaceHolder: Boolean = throwIfNoResetPlaceHolder

  override def toString: String = {
    val strings = List[String](
      s"fuzzSamples: $fuzzSamples",
      s"algorithm: $algorithm",
      s"algorithmParameter: $algorithmParameter",
      s"threads: $threads",
      s"userProvidedInputs: $useProvidedInputs",
      s"throwIfNoResetPlaceHolder: $throwIfNoResetPlaceHolder",
    )
    strings.mkString("\n")
  }
}