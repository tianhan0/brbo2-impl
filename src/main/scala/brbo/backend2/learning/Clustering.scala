package brbo.backend2.learning

import brbo.backend2.learning.TraceClustering.matrixToJsonString
import brbo.common.MyLogger
import org.apache.commons.io.IOUtils
import play.api.libs.json.Json

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, SECONDS}

object Clustering {
  private val CLUSTER_SCRIPT_DIRECTORY = {
    val separator = File.separator
    s"${System.getProperty("user.dir")}${separator}src${separator}main${separator}python"
  }
  private val CLUSTER_SCRIPT = s"clustering.py"
  private val TIMEOUT_IN_SECONDS = 30

  sealed trait Algorithm {
    def commandLineOption: String
  }

  case class Optics(maxEps: Option[Int]) extends Algorithm {
    def commandLineOption: String = {
      maxEps match {
        case Some(maxEps) => s"--algorithm=optics --max-eps=$maxEps"
        case None => ""
      }
    }
  }

  object KNN extends Algorithm {
    override def commandLineOption: String = "--algorithm=knn"
  }

  def cluster(distanceMatrix: List[List[Int]], algorithm: Algorithm, debugMode: Boolean): List[Int] = {
    val logger = if (debugMode) MyLogger.commonDebugLogger else MyLogger.commonLogger
    val inputFilePath = Files.createTempFile("", ".json").toAbsolutePath.toString
    logger.info(s"Write data into $inputFilePath")
    new PrintWriter(inputFilePath) {
      val inputFileContent: String = matrixToJsonString(distanceMatrix)
      logger.traceOrError(s"Input file content: $inputFileContent")
      write(inputFileContent)
      close()
    }
    val outputFile = Files.createTempFile("", ".json")

    val command = {
      s"python3 $CLUSTER_SCRIPT --input=$inputFilePath --output=$outputFile ${algorithm.commandLineOption}"
    }
    val processBuilder: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(command.split(" ").toList.asJava)
      .directory(new java.io.File(CLUSTER_SCRIPT_DIRECTORY))
      .redirectErrorStream(true)
    logger.info(s"Run python clustering script via `$command`")
    val process: java.lang.Process = processBuilder.start()
    if (process.waitFor(Duration(TIMEOUT_IN_SECONDS, SECONDS).toSeconds, TimeUnit.SECONDS)
      && process.exitValue() == 0) {
      logger.info(s"Read labels from ${outputFile.toAbsolutePath}")
      val outputFileContents = Files.readString(outputFile)
      logger.traceOrError(s"Output file content: $outputFileContents")
      val parsed = Json.parse(outputFileContents)
      parsed("labels").as[List[Int]]
    }
    else {
      val stdout = IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
      logger.fatal(s"Failed to execute $CLUSTER_SCRIPT. stdout: $stdout")
      logger.fatal(s"Letting all elements have the same label")
      List.fill(distanceMatrix.length)(0)
    }
  }
}
