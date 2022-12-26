package brbo.backend2.qfuzz

import brbo.BrboMain
import brbo.common.MyLogger
import brbo.common.ast.BrboProgram
import brbo.common.commandline.FuzzingArguments
import org.apache.commons.io.FileUtils

import java.io.File
import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Executor {
  private val FUZZ_WORKING_DIRECTORY = s"${System.getProperty("user.dir")}/src/main/java/brbo/fuzz"
  private val QFUZZ_PATH = s"${System.getProperty("user.home")}/Documents/workspace/qfuzz"
  private val KELINCI_JAR_PATH = s"$QFUZZ_PATH/tool/instrumentor/build/libs/kelinci.jar"
  private val BINARY_PATH = s"$FUZZ_WORKING_DIRECTORY/bin"
  private val INSTRUMENTED_BINARY_PATH = s"$FUZZ_WORKING_DIRECTORY/bin-instr"

  private val AFL_TIMEOUT_IN_SECONDS = 60
  private val KELINCI_TIMEOUT_IN_SECONDS = AFL_TIMEOUT_IN_SECONDS + 5

  def run(program: BrboProgram, arguments: FuzzingArguments): Unit = {
    val logger = MyLogger.createLogger(Executor.getClass, debugMode = arguments.getDebugMode)

    logger.info(s"Step 1: Prepare a QFuzz driver")
    val driverFileContents = DriverGenerator.run(program)
    val driverFilePath = {
      val driverClassName = DriverGenerator.driverClassName(program.className)
      s"$FUZZ_WORKING_DIRECTORY/$driverClassName.java"
    }
    BrboMain.writeToFile(driverFilePath, driverFileContents)
    logger.info(s"Step 1: Written into `$driverFilePath`")

    logger.info(s"Step 2: Compile and instrument the QFuzz driver")
    logger.info(s"Step 2.1: Compile the QFuzz driver")
    prepareDirectory(BINARY_PATH)
    BrboMain.executeCommandWithLogger(
      // IMPORTANT: Avoid using quotes when specifying class path
      // To debug, add "-verbose" option and check the class paths
      command = s"""javac -cp .:$KELINCI_JAR_PATH $driverFilePath -d $FUZZ_WORKING_DIRECTORY/bin""",
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
        logger.info(s"Step 3: Run Kelinci server")
        val driverFullyQualifiedClassName = DriverGenerator.driverFullyQualifiedClassName(program.className)
        BrboMain.executeCommandWithLogger(
          command = s"""java -cp .:$KELINCI_JAR_PATH:$FUZZ_WORKING_DIRECTORY/bin-instr/ edu.cmu.sv.kelinci.Kelinci -K 100 $driverFullyQualifiedClassName @@""",
          logger,
          timeout = KELINCI_TIMEOUT_IN_SECONDS
        )
      }
      case "afl" => Future {
        logger.info(s"Step 4: Prepare inputs of interest")
        // Ensure kelinci server starts first
        BrboMain.executeCommand(command = "sleep 2")
        BrboMain.executeCommandWithLogger(
          command = s"""$QFUZZ_PATH/tool/afl-2.51b-qfuzz/afl-fuzz -i $FUZZ_WORKING_DIRECTORY/inputs -o $FUZZ_WORKING_DIRECTORY/fuzzer-out -c quantify -K 100 -S afl -t 999999999 $QFUZZ_PATH/tool/fuzzerside/interface -K 100 @@""",
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
  }

  def prepareDirectory(directory: String): Unit = {
    val path = Paths.get(directory)
    if (Files.exists(path))
      FileUtils.deleteDirectory(new File(directory))
    Files.createDirectory(path)
  }
}
