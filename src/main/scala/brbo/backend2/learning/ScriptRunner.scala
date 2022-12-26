package brbo.backend2.learning

import brbo.BrboMain
import brbo.common.MyLogger
import org.apache.commons.io.IOUtils

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration

object ScriptRunner {
  private val OUTPUT_DIRECTORY = {
    val separator = File.separator
    s"${System.getProperty("user.dir")}${separator}src${separator}main${separator}python"
  }
  private val CLUSTER_SCRIPT = s"clustering.py"
  private val CLASSIFY_SCRIPT = s"classifier.py"
  private val TIMEOUT_IN_SECONDS = 1200
  private val TIMEOUT_DURATION = Duration(TIMEOUT_IN_SECONDS, TimeUnit.SECONDS).toSeconds

  sealed trait Algorithm {
    def commandLineOption: String

    def scriptName: String
  }

  abstract class Metric {
    def print(): String
  }

  object Precomputed extends Metric {
    override def print(): String = "precomputed"
  }

  object Euclidean extends Metric {
    override def print(): String = "euclidean"
  }

  case class Optics(maxEps: Option[Double], metric: Metric) extends Algorithm {
    def commandLineOption: String = {
      val algorithm = s"--algorithm=optics --metric=${metric.print()}"
      maxEps match {
        case Some(maxEps) => s"$algorithm --max-eps=$maxEps"
        case None => s"$algorithm"
      }
    }

    def scriptName: String = CLUSTER_SCRIPT
  }

  object KNN extends Algorithm {
    override def commandLineOption: String = "--algorithm=knn"

    def scriptName: String = CLUSTER_SCRIPT
  }

  case class KMeans(clusters: Option[Int]) extends Algorithm {
    override def commandLineOption: String = {
      val algorithm = "--algorithm=kmeans"
      clusters match {
        case Some(clusters) => s"$algorithm --clusters=$clusters"
        case None => s"$algorithm"
      }
    }

    def scriptName: String = CLUSTER_SCRIPT
  }

  case class DecisionTreeLearning(printTree: Boolean) extends Algorithm {
    override def commandLineOption: String = {
      if (printTree) s"--print-tree true"
      else ""
    }

    def scriptName: String = CLASSIFY_SCRIPT
  }

  def run(inputFileContent: String, algorithm: Algorithm, debugMode: Boolean): Option[String] = {
    val logger = if (debugMode) MyLogger.sharedDebugLogger else MyLogger.sharedLogger
    val inputFile = Files.createTempFile("", ".json")
    val inputFilePath = inputFile.toAbsolutePath.toString
    // logger.traceOrError(s"Write data into $inputFilePath")
    new PrintWriter(inputFilePath) {
      // logger.traceOrError(s"Input file content: $inputFileContent")
      write(inputFileContent)
      close()
    }
    val outputFile = Files.createTempFile("", ".json")

    val command = {
      s"python3 ${algorithm.scriptName} --input=$inputFilePath --output=$outputFile ${algorithm.commandLineOption}"
    }
    val processBuilder: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(command.split(" ").toList.asJava)
      .directory(new java.io.File(OUTPUT_DIRECTORY))
      .redirectErrorStream(true)
    val process: java.lang.Process = processBuilder.start()
    if (process.waitFor(TIMEOUT_DURATION, TimeUnit.SECONDS) && process.exitValue() == 0) {
      // logger.traceOrError(s"Read labels from ${outputFile.toAbsolutePath}")
      val outputFileContents = BrboMain.readFromFile(outputFile.toAbsolutePath.toString)
      // logger.traceOrError(s"Output file content: $outputFileContents")
      Files.deleteIfExists(inputFile)
      Files.deleteIfExists(outputFile)
      Some(outputFileContents)
    }
    else {
      val stdout = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
      logger.fatal(s"Ran python clustering script via `$command`")
      logger.fatal(s"Exit code ${process.exitValue()}. Failed to execute ${algorithm.scriptName}. stdout: $stdout")
      Files.deleteIfExists(outputFile)
      None
    }
  }
}
