package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.common.MyLogger
import org.apache.commons.io.IOUtils
import play.api.libs.json.Json

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, SECONDS}

object TraceClustering {
  private val logger = MyLogger.createLogger(TraceClustering.getClass, debugMode = false)
  private val CLUSTER_SCRIPT_DIRECTORY = {
    val separator = File.separator
    s"${System.getProperty("user.dir")}${separator}src${separator}main${separator}python"
  }
  private val CLUSTER_SCRIPT = s"clustering.py"

  def distanceMatrix(traces: List[Interpreter.CostTrace], substitutionPenalty: Int): List[List[Int]] = {
    traces.map({
      left =>
        traces.map({
          right => distance(left, right, substitutionPenalty)
        })
    })
  }

  def matrixToJson(distanceMatrix: List[List[Int]]): String = {
    val jsonObject = Json.obj(("data", distanceMatrix))
    jsonObject.toString()
  }

  def cluster(matrix: List[List[Int]]): List[Int] = {
    val inputFilePath = Files.createTempFile("", ".json").toAbsolutePath.toString
    new PrintWriter(inputFilePath) {
      write(matrixToJson(matrix))
      close()
    }
    val outputFile = Files.createTempFile("", ".json")

    val command = s"python3 $CLUSTER_SCRIPT --input=$inputFilePath --output=$outputFile"
    val processBuilder: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(command.split(" ").toList.asJava)
      .directory(new java.io.File(CLUSTER_SCRIPT_DIRECTORY))
      .redirectErrorStream(true)
    logger.info(s"Run python cluster script via `$command`")
    val process: java.lang.Process = processBuilder.start()
    if (process.waitFor(Duration(10, SECONDS).toSeconds, TimeUnit.SECONDS)) {
      val outputFileContents = Files.readString(outputFile)
      val parsed = Json.parse(outputFileContents)
      parsed("labels").as[List[Int]]
    }
    else {
      val stdout = {
        try {
          IOUtils.toString(process.getInputStream, StandardCharsets.UTF_8)
        }
        catch {
          case _: Exception => "stdout is empty (Since the process timed out)"
        }
      }
      throw new Exception(s"$CLUSTER_SCRIPT timed out. stdout: $stdout")
    }
  }

  private def distance(left: Interpreter.CostTrace, right: Interpreter.CostTrace, substitutionPenalty: Int): Int = {
    // Since permuting a trace has no cost, we eliminate the orders from traces
    val leftCostTrace: Set[String] = costTraceToSet(left)
    val rightCostTrace: Set[String] = costTraceToSet(right)
    // Common elements between traces have no cost. For example, between {x} and {x, y}, x is the common element and
    // hence we can ignore it when computing the distance
    val diff1 = leftCostTrace.diff(rightCostTrace)
    val diff2 = rightCostTrace.diff(leftCostTrace)
    // For uncommon elements (e.g., {x, y} and {a, b, c}), we first substitute (e.g., x becomes a and y becomes c)
    // and then insert (e.g., insert c)
    Math.min(diff1.size, diff2.size) * substitutionPenalty
  }

  private def costTraceToSet(trace: Interpreter.CostTrace): Set[String] = {
    trace.nodes.foldLeft(Set(): Set[String])({
      case (soFar, node) => node match {
        case Interpreter.UseNode(use, _) => soFar + s"${use.printToIR()}${use.uuid}"
        case _ => soFar
      }
    })
  }
}
