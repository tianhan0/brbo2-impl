package brbo.backend2.learning

import brbo.common.MyLogger
import org.apache.commons.io.IOUtils

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, SECONDS}

object ScriptRunner {
  private val OUTPUT_DIRECTORY = {
    val separator = File.separator
    s"${System.getProperty("user.dir")}${separator}src${separator}main${separator}python"
  }
  private val CLUSTER_SCRIPT = s"clustering.py"
  private val CLASSIFY_SCRIPT = s"classifier.py"
  private val TIMEOUT_IN_SECONDS = 30

  sealed trait Algorithm {
    def commandLineOption: String

    def scriptName: String
  }

  case class Optics(maxEps: Option[Int]) extends Algorithm {
    def commandLineOption: String = {
      val algorithm = "--algorithm=optics"
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
    val logger = if (debugMode) MyLogger.commonDebugLogger else MyLogger.commonLogger
    val inputFilePath = Files.createTempFile("", ".json").toAbsolutePath.toString
    logger.info(s"Write data into $inputFilePath")
    new PrintWriter(inputFilePath) {
      logger.traceOrError(s"Input file content: $inputFileContent")
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
    logger.info(s"Run python clustering script via `$command`")
    val process: java.lang.Process = processBuilder.start()
    if (process.waitFor(Duration(TIMEOUT_IN_SECONDS, SECONDS).toSeconds, TimeUnit.SECONDS)
      && process.exitValue() == 0) {
      logger.info(s"Read labels from ${outputFile.toAbsolutePath}")
      val outputFileContents = Files.readString(outputFile)
      logger.traceOrError(s"Output file content: $outputFileContents")
      Some(outputFileContents)
    }
    else {
      val stdout = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
      logger.fatal(s"Failed to execute ${algorithm.scriptName}. stdout: $stdout")
      None
    }
  }
}
