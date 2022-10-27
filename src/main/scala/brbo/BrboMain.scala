package brbo

import brbo.backend2.Driver
import brbo.common.CommandLineArguments.DEFAULT_ASSERTION_TAG
import brbo.common.cfg.ControlFlowGraph
import brbo.common.string.StringFormatUtils
import brbo.common.{CommandLineArguments, MyLogger}
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.apache.commons.io.{FileUtils, FilenameUtils}

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.JavaConverters._

object BrboMain {
  val OUTPUT_DIRECTORY: String = s"${System.getProperty("user.dir")}/output"
  val TOOL_NAME = "brbo2"

  private val BATCH_SIZE = 100

  def main(args: Array[String]): Unit = {
    val arguments = CommandLineArguments.parseArguments(args)
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
                       totalFiles: Int, date: String, innerMostPackageName: String, arguments: CommandLineArguments): Unit = {
    val batchString = s"${StringFormatUtils.integer(batchIndex * BATCH_SIZE, 3)}-${StringFormatUtils.integer((batchIndex + 1) * BATCH_SIZE - 1, 3)}"
    logger.info(s"Run `$batchIndex`-th batch`: $batchString")

    sourceFiles.zipWithIndex.foreach({
      case ((sourceFile: File, sourceFileContents: String), index) =>
        val fileIndex = index + batchIndex * BATCH_SIZE
        val progress: Double = fileIndex.toDouble / totalFiles * 100
        logger.info(s"Verify `$fileIndex`-th input file. Progress: ${StringFormatUtils.float(progress, 2)}%")
        analyze(logger, sourceFile.getAbsolutePath, sourceFileContents, arguments)
      // TODO: Store results into csv files
    })
  }

  def analyze(logger: MyLogger, sourceFilePath: String, sourceFileContents: String, arguments: CommandLineArguments): Unit = {
    logger.info(s"Analyze file `$sourceFilePath`")

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
      if (arguments.getPrintCFG) {
        ControlFlowGraph.toControlFlowGraph(targetProgram.program).printPDF()
      }
      val driver = new Driver(arguments, targetProgram.program)
      val assertionsToVerify = {
        arguments.getAssertionIndex match {
          case DEFAULT_ASSERTION_TAG =>
            logger.info(s"Verify every assertion in every file")
            targetProgram.program.boundAssertions
          case assertionTag =>
            logger.info(s"Verify the assertion associated with tag `$assertionTag` in every file")
            targetProgram.program.boundAssertions.filter(a => a.tag == assertionTag)
        }
      }
      assertionsToVerify.zipWithIndex.foreach({
        case (assertion, index) =>
          val progress: Double = (index + 1) / assertionsToVerify.size * 100
          logger.info(s"Verify assertion(s) with tag `${assertion.tag}` in every file [${index + 1}/${assertionsToVerify.size}]. Progress: ${StringFormatUtils.float(progress, 2)}%")
          driver.verify(assertion)
      })
    }
    else {
      logger.info(s"Skipping bound checking for file `$sourceFilePath`")
    }
  }

  private def readFromFile(location: String): String = {
    val source = scala.io.Source.fromFile(location)
    try source.mkString finally source.close()
  }
}
