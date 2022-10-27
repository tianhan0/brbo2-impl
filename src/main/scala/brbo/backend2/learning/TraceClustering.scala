package brbo.backend2.learning

import brbo.backend2.interpreter.Interpreter
import brbo.backend2.interpreter.Interpreter.{CostTrace, Trace}
import brbo.common.{DisjointSet, MyLogger}
import org.apache.commons.io.IOUtils
import org.jgrapht.alg.util.UnionFind
import play.api.libs.json.Json

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.{Duration, SECONDS}

object TraceClustering {
  private val CLUSTER_SCRIPT_DIRECTORY = {
    val separator = File.separator
    s"${System.getProperty("user.dir")}${separator}src${separator}main${separator}python"
  }
  private val CLUSTER_SCRIPT = s"clustering.py"

  def cluster(distanceMatrix: List[List[Int]], debugMode: Boolean): List[Int] = {
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

    val command = s"python3 $CLUSTER_SCRIPT --input=$inputFilePath --output=$outputFile"
    val processBuilder: java.lang.ProcessBuilder = new java.lang.ProcessBuilder(command.split(" ").toList.asJava)
      .directory(new java.io.File(CLUSTER_SCRIPT_DIRECTORY))
      .redirectErrorStream(true)
    logger.info(s"Run python cluster script via `$command`")
    val process: java.lang.Process = processBuilder.start()
    if (process.waitFor(Duration(30, SECONDS).toSeconds, TimeUnit.SECONDS) && process.exitValue() == 0) {
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

  def groupZeroDistanceTraces(traces: List[CostTrace]): Map[CostTrace, Set[CostTrace]] = {
    val disjointSet = new DisjointSet[CostTrace](
      (left, right) => distance(left, right, substitutionPenalty = 1) == 0,
      betterRepresentative
    )
    traces.foreach(t => disjointSet.add(t))

    disjointSet.getMap
  }

  def selectRepresentativeCostTrace(traces: List[CostTrace]): CostTrace = {
    traces.sortWith({
      case (left, right) => betterRepresentative(left, right)
    }).head
  }

  private def betterRepresentative(left: CostTrace, right: CostTrace): Boolean = {
    val (diff1, diff2) = distanceInternal(left, right)
    // If left is {x, y} and right is {y, z, w}, then right is a better representative because its decomposition
    // is more easily applicable to left, by treating x as z (or w)
    diff1.size >= diff2.size
  }

  def selectRepresentativeTrace(traces: List[Trace]): Trace = {
    val selectedCostTrace = selectRepresentativeCostTrace(traces.map(t => t.costTrace))
    traces.find(t => t.costTrace == selectedCostTrace).get
  }

  def distanceMatrix(traces: List[CostTrace], substitutionPenalty: Int): List[List[Int]] = {
    traces.map({
      left =>
        traces.map({
          right => distance(left, right, substitutionPenalty)
        })
    })
  }

  def matrixToJsonString(distanceMatrix: List[List[Int]]): String = {
    val jsonObject = Json.obj(("data", distanceMatrix))
    jsonObject.toString()
  }

  private def distance(left: CostTrace, right: CostTrace, substitutionPenalty: Int): Int = {
    val (diff1, diff2) = distanceInternal(left, right)
    Math.min(diff1.size, diff2.size) * substitutionPenalty
  }

  private def distanceInternal(left: CostTrace, right: CostTrace): (Set[String], Set[String]) = {
    // Since permuting a trace has no cost, we eliminate the orders from traces
    val leftCostTrace: Set[String] = costTraceToSet(left)
    val rightCostTrace: Set[String] = costTraceToSet(right)
    // Common elements between traces have no cost. For example, between {x} and {x, y}, x is the common element and
    // hence we can ignore it when computing the distance
    val diff1 = leftCostTrace.diff(rightCostTrace)
    val diff2 = rightCostTrace.diff(leftCostTrace)
    // For uncommon elements (e.g., {x, y} and {a, b, c}), we first substitute (e.g., x becomes a and y becomes c)
    // and then insert (e.g., insert c)
    (diff1, diff2)
  }

  private def costTraceToSet(trace: CostTrace): Set[String] = {
    trace.nodes.foldLeft(Set(): Set[String])({
      case (soFar, node) => node match {
        case Interpreter.UseNode(use, _) => soFar + s"${use.printToIR()}${use.uuid}"
        case _ => soFar
      }
    })
  }
}
