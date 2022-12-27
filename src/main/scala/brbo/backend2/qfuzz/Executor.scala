package brbo.backend2.qfuzz

import brbo.BrboMain
import brbo.backend2.InputParser
import brbo.common.ast._
import brbo.common.commandline.FuzzingArguments
import brbo.common.{BrboType, MyLogger}
import org.apache.commons.io.{FileUtils, FilenameUtils}
import play.api.libs.json.JsArray

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.nio.{BufferUnderflowException, ByteBuffer}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Executor {
  def run(program: BrboProgram, sourceFilePath: String, arguments: FuzzingArguments): Unit = {
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
        val command = s"""java -cp .:$KELINCI_JAR_PATH:$TEMPORARY_DIRECTORY/bin-instr/ edu.cmu.sv.kelinci.Kelinci -K 100 $driverFullyQualifiedClassName @@"""
        logger.info(s"Execute `$command`")
        val kelinciOutput = BrboMain.executeCommand(command = command, timeout = KELINCI_TIMEOUT_IN_SECONDS)
        val outputDirectory = s"${BrboMain.OUTPUT_DIRECTORY}/fuzz"
        Files.createDirectories(Paths.get(outputDirectory))
        BrboMain.writeToFile(s"$outputDirectory/kelinci_output.txt", kelinciOutput)
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
    val pathCostFileContents = BrboMain.readFromFile(s"$FUZZ_OUT_DIRECTORY/afl/path_costs.csv")
    logger.info(s"QFuzz output:\n$pathCostFileContents")
    val interestingInputFiles = pathCostFileContents.split("\n").filter({
      line => line.contains("+partition")
    }).map({
      line =>
        val items = line.split(";")
        items(1).trim
    })
    logger.info(s"Interesting inputs:\n${interestingInputFiles.mkString("\n")}")

    logger.info(s"Step 5: Parse the interesting inputs")
    val listOfInputs = FileUtils.listFiles(new File(FUZZ_OUT_DIRECTORY), null, true).asScala.flatMap({
      file =>
        val absolutePath = file.getAbsolutePath
        if (absolutePath.contains(s"$FUZZ_OUT_DIRECTORY/afl/queue/id") && file.isFile) {
          if (interestingInputFiles.exists(interestingInput => absolutePath.contains(interestingInput))) {
            logger.info(s"Read shorts that are between [${DriverGenerator.MIN_INTEGER}, ${DriverGenerator.MAX_INTEGER}] from $absolutePath")
            val shorts = parseBytesToShorts(absolutePath)
            val inputs = wrapShorts(shorts)
            logger.info(s"Inputs: $inputs.")
            Some(inputs)
          } else {
            logger.info(s"Not an interesting input: $absolutePath")
            None
          }
        } else {
          logger.info(s"Not an input file: $absolutePath (Is file? ${file.isFile})")
          None
        }
    })
    val interestingInputs = JsArray(
      listOfInputs.map({
        inputs =>
          val inputValues = toInputValues(inputs, program.mainFunction.parameters)
          JsArray(inputValues.map(inputValue => InputParser.toJson(inputValue)))
      }).toList
    )
    val inputFilePath = getInputFilePath(sourceFilePath)
    logger.info(s"Step 5: Write interesting inputs into file $inputFilePath")
    BrboMain.writeToFile(path = inputFilePath, content = interestingInputs.toString())

    FileUtils.deleteDirectory(new File(FUZZ_OUT_DIRECTORY))
    FileUtils.deleteDirectory(new File(BINARY_PATH))
    FileUtils.deleteDirectory(new File(INSTRUMENTED_BINARY_PATH))
  }

  def getInputFilePath(sourceFilePath: String): String = {
    assert(FilenameUtils.getExtension(sourceFilePath) == "java")
    val fileName = s"${FilenameUtils.getBaseName(sourceFilePath)}_fuzzing.json"
    s"${FilenameUtils.getFullPath(sourceFilePath)}$fileName"
  }

  def toInputValues(inputArray: List[Int], parameters: List[Identifier]): List[BrboValue] = {
    val (_, inputValues) = parameters.foldLeft(0, Nil: List[BrboValue])({
      case ((indexSoFar, inputValues), parameter) =>
        parameter.typ match {
          case BrboType.INT =>
            (indexSoFar + 1, Number(inputArray(indexSoFar)) :: inputValues)
          case BrboType.ARRAY(BrboType.INT) =>
            val arrayValue: List[Int] = inputArray.slice(indexSoFar, indexSoFar + DriverGenerator.ARRAY_SIZE)
            val inputValue = BrboArray(values = arrayValue.map(v => Number(v)), innerType = BrboType.INT)
            (indexSoFar + DriverGenerator.ARRAY_SIZE, inputValue :: inputValues)
          case _ => throw new Exception(s"Not support a parameter of type ${parameter.typ}")
        }
    })
    inputValues.reverse
  }

  def prepareDirectory(directory: String): Unit = {
    val path = Paths.get(directory)
    if (Files.exists(path))
      FileUtils.deleteDirectory(new File(directory))
    Files.createDirectory(path)
  }

  private def parseBytesToShorts(path: String): List[java.lang.Short] = {
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

  private def wrapShorts(shorts: List[java.lang.Short],
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
