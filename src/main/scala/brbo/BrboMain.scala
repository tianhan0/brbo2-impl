package brbo

import brbo.backend2.DecompositionDriver
import brbo.backend2.qfuzz.Executor
import brbo.common.MyLogger
import brbo.common.cfg.ControlFlowGraph
import brbo.common.commandline._
import brbo.common.string.StringFormatUtils
import brbo.frontend.{BasicProcessor, TargetProgram}
import org.apache.commons.io.{FileUtils, FilenameUtils, IOUtils}

import java.io.{File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._

object BrboMain {
  val OUTPUT_DIRECTORY: String = s"${System.getProperty("user.dir")}/output"
  val TOOL_NAME = "brbo2"
  private val BATCH_SIZE = 100

  def main(args: Array[String]): Unit = {
    val arguments = parseArguments(args)
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

    // val date = new SimpleDateFormat("MMdd-HHmm").format(new Date) // YYYYMMdd-HHmm
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
          case (batch, index) => runBatch(logger, batch, index, sourceFiles.size, arguments)
        })
    })
    sys.exit()
  }

  private def parseArguments(args: Array[String]): CommonArguments = {
    if (args.isEmpty) {
      println(s"Expect a subcommand from ${Subcommand.Fuzz.toString}, ${Subcommand.Decompose.toString}")
      sys.exit(status = 0)
    }
    val subcommand = {
      if (args.head == Subcommand.Fuzz.toString) Subcommand.Fuzz
      else if (args.head == Subcommand.Decompose.toString) Subcommand.Decompose
      else throw new Exception
    }
    CommonArguments.parseArguments(args.tail, subcommand)
  }

  private def runBatch(logger: MyLogger, sourceFiles: List[(File, String)], batchIndex: Int,
                       totalFiles: Int, arguments: CommonArguments): Unit = {
    val batchString = s"${StringFormatUtils.integer(batchIndex * BATCH_SIZE, 3)}-${StringFormatUtils.integer((batchIndex + 1) * BATCH_SIZE - 1, 3)}"
    logger.info(s"Run `$batchIndex`-th batch`: $batchString")

    sourceFiles.zipWithIndex.foreach({
      case ((sourceFile: File, sourceFileContents: String), index) =>
        val fileIndex = index + batchIndex * BATCH_SIZE
        val progress: Double = fileIndex.toDouble / totalFiles * 100
        logger.info(s"Process `$fileIndex`-th input file. Progress: ${StringFormatUtils.float(progress, 2)}%")
        val sourceFilePath = sourceFile.getAbsolutePath
        logger.info(s"Process file `$sourceFilePath`")
        val className: String = getClassName(sourceFilePath)
        logger.info(s"Class name: `$className`")
        if (className == "brbo.benchmarks.Common") {
          logger.info(s"Skip file `$sourceFilePath`")
          sys.exit()
        }
        val targetProgram = BasicProcessor.getTargetProgram(className, sourceFileContents)

        arguments match {
          case arguments: DecompositionArguments =>
            decompose(targetProgram, logger, sourceFilePath, arguments)
            logger.info(s"Finished decomposing ${sourceFile.getAbsolutePath}")
          // TODO: Store results into csv files
          case arguments: FuzzingArguments =>
            fuzzing(targetProgram, arguments)
          case _ => throw new Exception
        }
    })
  }

  def fuzzing(targetProgram: TargetProgram, arguments: FuzzingArguments): Unit = {
    Executor.run(targetProgram.program, arguments)
  }

  def decompose(targetProgram: TargetProgram,
                logger: MyLogger,
                sourceFilePath: String,
                arguments: DecompositionArguments): Unit = {
    logger.info(s"Parsing...")
    if (arguments.getDebugMode)
      ControlFlowGraph.toControlFlowGraph(targetProgram.program).printPDF()
    val driver = new DecompositionDriver(
      arguments = arguments,
      program = targetProgram.program,
      inputFilePath = DecompositionDriver.getInputFilePath(useProvidedInputs = arguments.getUseProvidedInputs, sourceFilePath)
    )
    val startTime = System.nanoTime
    val decomposedProgram = driver.decompose()
    val newDecomposition = decomposedProgram.printToBrboJava(indent = 0)
    val endTime = System.nanoTime
    val outputPath = {
      val parent = FilenameUtils.getBaseName(Paths.get(sourceFilePath).getParent.toAbsolutePath.toString)
      Paths.get(OUTPUT_DIRECTORY, "decomposed", parent, s"${FilenameUtils.getBaseName(sourceFilePath)}.java")
    }
    Files.createDirectories(outputPath.getParent)
    // val duration = (endTime - startTime) / 1e9d
    // val statistics = getStatistics(duration, arguments, driver.getNumberOfTraces)
    if (Files.exists(outputPath)) {
      val existingDecomposition = readFromFile(outputPath.toAbsolutePath.toString)
      val actualOutputPath = Paths.get(outputPath.toAbsolutePath.toString + ".actual")
      Files.deleteIfExists(actualOutputPath)
      if (existingDecomposition != newDecomposition) {
        logger.info(s"Write into file $actualOutputPath")
        writeToFile(actualOutputPath, newDecomposition)

        logger.info(s"New decomposition differs from the existing decomposition")
        logger.info(diffFiles(outputPath, actualOutputPath))
      }
    } else {
      logger.info(s"Write into file $outputPath")
      writeToFile(outputPath, newDecomposition)
    }
  }

  private def getClassName(sourceFilePath: String): String = {
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

  private def getStatistics(duration: Double, arguments: DecompositionArguments, numberOfTraces: Int): String = {
    s"// duration,numberOfTraces,fuzzSamples,algorithm,\n" +
      s"// $duration,$numberOfTraces,${arguments.getFuzzSamples},${arguments.getAlgorithm}"
  }

  def diffFiles(file1: Path, file2: Path): String =
    executeCommand(s"diff -u ${file1.toString} ${file2.toString}", environment = Map())

  def readFromFile(path: String): String = {
    val source = scala.io.Source.fromFile(path)
    try source.mkString finally source.close()
  }

  def writeToFile(path: String, content: String): Unit = writeToFile(Paths.get(path), content)

  def writeToFile(path: Path, content: String): Unit = {
    Files.deleteIfExists(path)
    Files.createFile(path)
    val fileWriter = new FileWriter(path.toAbsolutePath.toString)
    fileWriter.write(content)
    fileWriter.close()
  }

  def executeCommandWithLogger(command: String,
                               logger: MyLogger,
                               environment: Map[String, String] = Map(),
                               timeout: Int = 5): Unit = {
    logger.info(s"Execute `$command`")
    logger.info(s"Output:\n${executeCommand(command, environment, timeout)}")
  }

  def executeCommand(command: String,
                     environment: Map[String, String] = Map(),
                     timeout: Int = 5): String = {
    val stderrFile = Files.createTempFile(Paths.get(OUTPUT_DIRECTORY), "stderr", ".txt")
    val stdoutFile = Files.createTempFile(Paths.get(OUTPUT_DIRECTORY), "stdout", ".txt")

    def getProcessOutput: String = {
      val stdout = FileUtils.readFileToString(new File(stdoutFile.toAbsolutePath.toString), StandardCharsets.UTF_8)
      val stderr = FileUtils.readFileToString(new File(stderrFile.toAbsolutePath.toString), StandardCharsets.UTF_8)
      stdout + "\n" + stderr
    }

    val processBuilder: java.lang.ProcessBuilder =
      new java.lang.ProcessBuilder(command.split(" ").toList.asJava)
        .redirectError(stderrFile.toFile)
        .redirectOutput(stdoutFile.toFile)
    environment.foreach({ case (key, value) => processBuilder.environment().put(key, value) })
    val process: java.lang.Process = processBuilder.start()
    // val environmentString = processBuilder.environment().asScala.map({ case (key, value) => s"$key: $value" })
    // println(environmentString.mkString("\n"))
    try {
      if (!process.waitFor(timeout, TimeUnit.SECONDS)) {
        process.destroyForcibly()
        getProcessOutput
      } else {
        getProcessOutput
      }
    } catch {
      case e: Exception => s"Ran into an exception:\n${e.toString}"
    } finally {
      Files.deleteIfExists(stderrFile)
      Files.deleteIfExists(stdoutFile)
    }
  }
}
