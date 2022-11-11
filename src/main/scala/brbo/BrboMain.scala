package brbo

import brbo.backend2.Driver
import brbo.common.cfg.ControlFlowGraph
import brbo.common.string.StringFormatUtils
import brbo.common.{MyLogger, NewCommandLineArguments}
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.apache.commons.io.{FileUtils, FilenameUtils}

import java.io.{File, FileWriter}
import java.nio.file.{Files, Path, Paths}
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.JavaConverters._

object BrboMain {
  val OUTPUT_DIRECTORY: String = s"${System.getProperty("user.dir")}/output"
  private val OUTPUT_DIRECTORY_PATH: Path = Paths.get(OUTPUT_DIRECTORY)
  val TOOL_NAME = "brbo2"

  private val BATCH_SIZE = 100

  def main(args: Array[String]): Unit = {
    val arguments = NewCommandLineArguments.parseArguments(args)
    val logger = MyLogger.createLogger(BrboMain.getClass, debugMode = arguments.getDebugMode)
    logger.info(s"$TOOL_NAME has started.")

    arguments.toString.split("\n").foreach(s => logger.info(s"Command line argument - $s"))
    logger.warn(s"We assume each class contains exactly one method named `${TargetProgram.MAIN_FUNCTION}`")

    val sourceFiles: List[(File, String)] = {
      val files: Array[File] = {
        val file = new java.io.File(arguments.getDirectoryToAnalyze)
        if (file.isDirectory) FileUtils.listFiles(file, Array("java"), true).asScala.toArray
        else Array(file)
      }
      val javaFilePaths: List[File] = files.filter(_.getName.endsWith(".java")).toList.sortWith({
        case (f1, f2) => f1.getAbsolutePath < f2.getAbsolutePath
      })
      javaFilePaths.map({
        sourceFileLocation =>
          logger.info(s"Read from source file `$sourceFileLocation`")
          (sourceFileLocation, readFromFile(sourceFileLocation.getAbsolutePath))
      })
    }

    val date = new SimpleDateFormat("MMdd-HHmm").format(new Date) // YYYYMMdd-HHmm
    logger.info(s"Group programs based on the inner most package names")
    val groups: Map[String, List[(File, String)]] = sourceFiles.groupBy({
      case (file, _) =>
        val absolutePath = file.getAbsolutePath
        val parentDirectory = absolutePath.substring(0, FilenameUtils.indexOfLastSeparator(absolutePath))
        val innerMostPackageName = parentDirectory.substring(FilenameUtils.indexOfLastSeparator(parentDirectory) + 1)
        innerMostPackageName
    })

    groups.foreach({
      case (innerMostPackageName, list) =>
        logger.info(s"Run $TOOL_NAME on files in package `$innerMostPackageName`")
        logger.info(s"Run $TOOL_NAME on programs in batches. Batch size: `$BATCH_SIZE`")
        list.grouped(BATCH_SIZE).zipWithIndex.foreach({
          case (batch, index) => runBatch(logger, batch, index, sourceFiles.size, date, innerMostPackageName, arguments)
        })
    })
  }

  private def runBatch(logger: MyLogger, sourceFiles: List[(File, String)], batchIndex: Int,
                       totalFiles: Int, date: String, innerMostPackageName: String, arguments: NewCommandLineArguments): Unit = {
    val batchString = s"${StringFormatUtils.integer(batchIndex * BATCH_SIZE, 3)}-${StringFormatUtils.integer((batchIndex + 1) * BATCH_SIZE - 1, 3)}"
    logger.info(s"Run `$batchIndex`-th batch`: $batchString")

    sourceFiles.zipWithIndex.foreach({
      case ((sourceFile: File, sourceFileContents: String), index) =>
        val fileIndex = index + batchIndex * BATCH_SIZE
        val progress: Double = fileIndex.toDouble / totalFiles * 100
        logger.info(s"Process `$fileIndex`-th input file. Progress: ${StringFormatUtils.float(progress, 2)}%")
        analyze(logger, sourceFile, sourceFileContents, arguments)
      // TODO: Store results into csv files
    })
  }

  def analyze(logger: MyLogger, sourceFile: File, sourceFileContents: String, arguments: NewCommandLineArguments): Unit = {
    val sourceFilePath = sourceFile.getAbsolutePath
    logger.info(s"Process file `$sourceFilePath`")

    val className: String = {
      val prefix = """src/main/java/"""
      val indexOfPrefix = sourceFilePath.indexOf(prefix)
      val almostClassName = {
        if (indexOfPrefix != -1) {
          sourceFilePath.substring(indexOfPrefix + prefix.length)
        }
        else sourceFilePath
      }
      val indexOfExtension = FilenameUtils.indexOfExtension(almostClassName)
      almostClassName.replace("""/""", ".").substring(0, indexOfExtension)
    }
    logger.info(s"Class name: `$className`")

    if (className != "brbo.benchmarks.Common") {
      logger.info(s"Parsing...")
      val targetProgram = BasicProcessor.getTargetProgram(className, sourceFileContents)
      if (arguments.getDebugMode) {
        ControlFlowGraph.toControlFlowGraph(targetProgram.program).printPDF()
      }
      val driver = new Driver(arguments, targetProgram.program)
      val startTime = System.nanoTime
      val decomposedProgram = driver.decompose()
      val endTime = System.nanoTime
      val outputPath = {
        val parent = FilenameUtils.getBaseName(Paths.get(sourceFilePath).getParent.toAbsolutePath.toString)
        val outputPath = Paths.get(OUTPUT_DIRECTORY, "decomposed", parent, s"${FilenameUtils.getBaseName(sourceFilePath)}.java")
        Files.deleteIfExists(outputPath)
        Files.createDirectories(outputPath.getParent)
        Files.createFile(outputPath)
        outputPath
      }
      val duration = (endTime - startTime) / 1e9d
      val statistics = getStatistics(duration, arguments, driver.getNumberOfTraces)
      val fileWriter = new FileWriter(outputPath.toAbsolutePath.toString)
      logger.info(s"Write into file $outputPath")
      fileWriter.write(decomposedProgram.printToJava() + s"\n$statistics")
      fileWriter.close()
    }
    else {
      logger.info(s"Not decompose `$sourceFilePath`")
    }
  }

  private def getStatistics(duration: Double, arguments: NewCommandLineArguments, numberOfTraces: Int): String = {
    s"// duration,numberOfTraces,fuzzSamples,algorithm,\n" +
      s"// $duration,$numberOfTraces,${arguments.getFuzzSamples},${arguments.getAlgorithm}"
  }

  private def readFromFile(location: String): String = {
    val source = scala.io.Source.fromFile(location)
    try source.mkString finally source.close()
  }
}
