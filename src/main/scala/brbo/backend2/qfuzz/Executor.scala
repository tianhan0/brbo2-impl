package brbo.backend2.qfuzz

import brbo.BrboMain
import brbo.common.MyLogger
import brbo.common.ast.BrboProgram
import brbo.common.commandline.FuzzingArguments
import org.apache.commons.io.FileUtils

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.nio.{BufferUnderflowException, ByteBuffer}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Executor {
  def run(program: BrboProgram, arguments: FuzzingArguments): Unit = {
    val logger = MyLogger.createLogger(Executor.getClass, debugMode = arguments.getDebugMode)

    val QFUZZ_PATH = arguments.getQFuzzPath
    val KELINCI_JAR_PATH = s"$QFUZZ_PATH/tool/instrumentor/build/libs/kelinci.jar"
    val AFL_TIMEOUT_IN_SECONDS = arguments.getAflTimeoutInSeconds
    val KELINCI_TIMEOUT_IN_SECONDS = AFL_TIMEOUT_IN_SECONDS + 5
    val TEMPORARY_DIRECTORY: Path = Paths.get(arguments.getOutputPath)
    val FUZZ_OUT_DIRECTORY = s"$TEMPORARY_DIRECTORY/fuzz-out"
    val BINARY_PATH = s"$TEMPORARY_DIRECTORY/bin"
    val INSTRUMENTED_BINARY_PATH = s"$TEMPORARY_DIRECTORY/bin-instr"
    val INPUT_SEED_PATH = arguments.getInputPath

    logger.info(s"Step 1: Prepare a QFuzz driver")
    val driverFileContents = DriverGenerator.run(program)
    val driverFilePath = {
      val driverClassName = DriverGenerator.driverClassName(program.className)
      s"$TEMPORARY_DIRECTORY/$driverClassName.java"
    }
    BrboMain.writeToFile(driverFilePath, driverFileContents)
    logger.info(s"Step 1: Written into `$driverFilePath`")

    logger.info(s"Step 2: Compile and instrument the QFuzz driver")
    logger.info(s"Step 2.1: Compile the QFuzz driver")
    prepareDirectory(BINARY_PATH)
    BrboMain.executeCommandWithLogger(
      // IMPORTANT: Avoid using quotes when specifying class path
      // To debug, add "-verbose" option and check the class paths
      command = s"""javac -cp .:$KELINCI_JAR_PATH $driverFilePath -d $TEMPORARY_DIRECTORY/bin""",
      logger
    )
    logger.info(s"Step 2.2: Instrument the QFuzz driver")
    prepareDirectory(INSTRUMENTED_BINARY_PATH)
    BrboMain.executeCommandWithLogger(
      command = s"""java -cp .:$KELINCI_JAR_PATH edu.cmu.sv.kelinci.instrumentor.Instrumentor -mode LABELS -i $BINARY_PATH -o $INSTRUMENTED_BINARY_PATH -skipmain""",
      logger
    )

    val commands = List("kelinci", "afl")
    val future = Future.traverse(commands)({
      case "kelinci" => Future {
        logger.info(s"Step 3: Run Kelinci server. Timeout: $KELINCI_TIMEOUT_IN_SECONDS sec.")
        val driverFullyQualifiedClassName = DriverGenerator.driverFullyQualifiedClassName(program.className)
        BrboMain.executeCommandWithLogger(
          command = s"""java -cp .:$KELINCI_JAR_PATH:$TEMPORARY_DIRECTORY/bin-instr/ edu.cmu.sv.kelinci.Kelinci -K 100 $driverFullyQualifiedClassName @@""",
          logger,
          timeout = KELINCI_TIMEOUT_IN_SECONDS
        )
      }
      case "afl" => Future {
        logger.info(s"Step 4: Run AFL. Output directory: $FUZZ_OUT_DIRECTORY. Timeout: $AFL_TIMEOUT_IN_SECONDS sec.")
        // Ensure kelinci server starts first
        // Remember to use https://github.com/litho17/qfuzz/commit/5dbe56efc6a9d9f77c320f1a8f801f6c4d6400e3
        BrboMain.executeCommandWithLogger(command = "sleep 3", logger)
        BrboMain.executeCommandWithLogger(
          command = s"""$QFUZZ_PATH/tool/afl-2.51b-qfuzz/afl-fuzz -i $INPUT_SEED_PATH -o $FUZZ_OUT_DIRECTORY -c quantify -K 100 -S afl -t 999999999 $QFUZZ_PATH/tool/fuzzerside/interface -K 100 @@""",
          logger,
          environment = Map(
            "AFL_SKIP_CPUFREQ" -> "1",
            "AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES" -> "1",
          ),
          timeout = AFL_TIMEOUT_IN_SECONDS
        )
      }
    })
    Await.result(future, Duration.Inf)
    logger.info(s"QFuzz output:\n${BrboMain.readFromFile(s"$FUZZ_OUT_DIRECTORY/afl/path_costs.csv")}")

    logger.info(s"Step 5: Parse the interesting inputs")
    FileUtils.listFiles(new File(FUZZ_OUT_DIRECTORY), null, true).asScala.foreach({
      file =>
        if (file.getAbsolutePath.startsWith(s"$FUZZ_OUT_DIRECTORY/afl/queue/id") && file.isFile) {
          logger.info(s"Read shorts that are between [${DriverGenerator.MIN_INTEGER}, ${DriverGenerator.MAX_INTEGER}] from ${file.getAbsolutePath}")
          val shorts = parseBytesToShorts(file.getAbsolutePath)
          val inputs = wrapShorts(shorts)
          logger.info(s"Inputs: $inputs")
        }
    })

    FileUtils.deleteDirectory(new File(FUZZ_OUT_DIRECTORY))
    FileUtils.deleteDirectory(new File(BINARY_PATH))
    FileUtils.deleteDirectory(new File(INSTRUMENTED_BINARY_PATH))
  }

  def prepareDirectory(directory: String): Unit = {
    val path = Paths.get(directory)
    if (Files.exists(path))
      FileUtils.deleteDirectory(new File(directory))
    Files.createDirectory(path)
  }

  def parseBytesToShorts(path: String): List[java.lang.Short] = {
    val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(path))
    byteArray.toList.grouped(java.lang.Short.BYTES).flatMap({
      short: List[Byte] =>
        try {
          val value: java.lang.Short = ByteBuffer.wrap(short.toArray).getShort()
          Some(value)
        } catch {
          case _: BufferUnderflowException => None
        }
    }).toList
  }

  def wrapShorts(shorts: List[java.lang.Short],
                 max: Int = DriverGenerator.MAX_INTEGER,
                 min: Int = DriverGenerator.MIN_INTEGER): List[Int] = {
    shorts.map({
      short =>
        var result: Int = if (short >= 0) short.toInt else -short
        result = result % (max - min + 1) + min
        result
    })
  }
}
