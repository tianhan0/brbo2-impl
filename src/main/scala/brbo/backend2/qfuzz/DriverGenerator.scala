package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.BrboType.QFuzzPrintType
import brbo.common.ast.{BrboProgram, Identifier}
import brbo.common.string.StringFormatUtils
import brbo.frontend.TargetProgram

object DriverGenerator {
  val ARRAY_SIZE = 8
  val MAX_INTEGER = 30
  val MIN_INTEGER = 1
  private val MAX_NUMBER_OF_USES_TO_TRACK = 1000

  def run(program: BrboProgram): String = {
    val (declarations, initializations, prints) = declarationsAndInitializations(program.mainFunction.parameters)
    val transformedProgram = ProgramTransformer.transform(program)
    s"""package $DRIVER_PACKAGE_NAME;
       |
       |import edu.cmu.sv.kelinci.Kelinci;
       |import edu.cmu.sv.kelinci.Mem;
       |import edu.cmu.sv.kelinci.quantification.Greedy;
       |import edu.cmu.sv.kelinci.quantification.PartitionAlgorithm;
       |import edu.cmu.sv.kelinci.quantification.PartitionSet;
       |
       |import java.io.FileInputStream;
       |import java.io.IOException;
       |import java.nio.ByteBuffer;
       |import java.util.ArrayList;
       |import java.util.Arrays;
       |import java.util.List;
       |
       |public class ${driverClassName(program.className)} {
       |  public final static int ARRAY_SIZE = $ARRAY_SIZE;
       |  private final static short MAX_INTEGER = $MAX_INTEGER;
       |  private final static short MIN_INTEGER = $MIN_INTEGER;
       |  private final static int MAX_NUMBER_OF_USES_TO_TRACK = $MAX_NUMBER_OF_USES_TO_TRACK;
       |
       |  /* Minimum distance between clusters. */
       |  public final static double epsilon = 1.0;
       |
       |  /* Cluster Algorithm */
       |  public static PartitionAlgorithm clusterAlgorithm = new Greedy(false);
       |
       |  public static void main(String[] arguments) {
       |    if (arguments.length != 1) {
       |      System.out.println("Expects file name as parameter");
       |      return;
       |    }
       |
       |    List<Short> values = new ArrayList<>();
       |    try (FileInputStream inputStream = new FileInputStream(arguments[0])) {
       |      System.out.printf("Read shorts between [%d, %d]\\n", MIN_INTEGER, MAX_INTEGER);
       |      byte[] bytes = new byte[Short.BYTES];
       |      while ((inputStream.read(bytes) != -1)) {
       |        short rawValue = ByteBuffer.wrap(bytes).getShort();
       |        if (rawValue == Short.MIN_VALUE)
       |          continue;
       |        short value = rawValue >= 0 ? rawValue : (short) -rawValue;
       |        value = (short) (value % (MAX_INTEGER - MIN_INTEGER + 1) + MIN_INTEGER);
       |        System.out.printf("Read value: %d (raw: %d)\\n", value, rawValue);
       |        values.add(value);
       |      }
       |    } catch (IOException e) {
       |      System.err.println("Error reading input");
       |      e.printStackTrace();
       |      return;
       |    }
       |
       |${prependIndents(declarations, indent = 4).mkString("\n")}
       |    try {
       |${prependIndents(initializations, indent = 6).mkString("\n")}
       |    } catch (IndexOutOfBoundsException exception) {
       |      long[] actualObservations = new long[0];
       |      PartitionSet clusters = PartitionSet.createFromObservations(epsilon, actualObservations, clusterAlgorithm);
       |      Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
       |      System.out.println("Not sufficient inputs!");
       |      return;
       |    }
       |${prependIndents(prints, indent = 4).mkString("\n")}
       |
       |    long[] observations = new long[MAX_NUMBER_OF_USES_TO_TRACK];
       |    ${program.className} program = new ${program.className}();
       |    Mem.clear(true);
       |    int useCount = 0;
       |    long lastMemInstrCost = 0L;
       |    int iThUse = 1;
       |    for (; iThUse <= MAX_NUMBER_OF_USES_TO_TRACK; iThUse++) {
       |      // In the i-th iteration, we collect accumulated resource consumption up to the secret[i]-th uses
       |      Mem.clear(false);
       |      useCount = program.${TargetProgram.MAIN_FUNCTION}(${program.mainFunction.parameters.map({ identifier => identifier.name }).mkString(", ")}, iThUse);
       |      if (iThUse <= useCount)
       |        System.out.printf("iThUse: %d; cost: %d; useCount: %d\\n", iThUse, Mem.instrCost, useCount);
       |      long thisCost = 0L;
       |      if (iThUse == 1) {
       |        thisCost = Mem.instrCost;
       |      } else {
       |        thisCost = Mem.instrCost - lastMemInstrCost;
       |      }
       |      assert (thisCost >= 0);
       |      observations[iThUse - 1] = thisCost;
       |      lastMemInstrCost = Mem.instrCost;
       |    }
       |    long[] actualObservations = new long[useCount];
       |    System.arraycopy(observations, 0, actualObservations, 0, actualObservations.length);
       |    System.out.println("observations: " + Arrays.toString(actualObservations));
       |
       |    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, actualObservations, clusterAlgorithm);
       |    // Give feedback to fuzzer: Number of clusters, min distance between clusters
       |    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
       |
       |    System.out.println("Done.");
       |  }
       |}
       |
       |${transformedProgram.printToQFuzzJava(indent = 0)}""".stripMargin
  }

  def driverClassName(className: String): String = s"${className}QFuzzDriver"

  def driverFullyQualifiedClassName(className: String): String = s"$DRIVER_PACKAGE_NAME.${driverClassName(className)}"

  private val DRIVER_PACKAGE_NAME = "brbo.fuzz.drivers"

  def prependIndents(lines: List[String], indent: Int): List[String] =
    lines.map(line => StringFormatUtils.prependIndentsPerLine(line, indent))

  def declarationsAndInitializations(parameters: List[Identifier]): (List[String], List[String], List[String]) = {
    val (_, declarations, initializations, prints) = parameters.foldLeft((0, Nil: List[String], Nil: List[String], Nil: List[String]))({
      case ((indexSoFar, declarations, initializations, prints), parameter) =>
        parameter.typ match {
          case BrboType.INT =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)};"
            val initialization = s"${parameter.name} = values.get($indexSoFar);"
            val print = s"""System.out.println("${parameter.name}: " + ${parameter.name});"""
            (indexSoFar + 1,
              declaration :: declarations,
              initialization :: initializations,
              print :: prints)
          case BrboType.ARRAY(BrboType.INT) =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)} = new int[ARRAY_SIZE];"
            val initialization =
              s"""for (int i = 0; i < ARRAY_SIZE && $indexSoFar + i < values.size(); i++) {
                 |  ${parameter.name}[i] = values.get($indexSoFar + i);
                 |}""".stripMargin
            val print = s"""System.out.println("${parameter.name}: " + Arrays.toString(${parameter.name}));"""
            (indexSoFar + ARRAY_SIZE,
              declaration :: declarations,
              initialization :: initializations,
              print :: prints)
          case BrboType.BOOL =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)};"
            val halfMaxValue = java.lang.Short.MAX_VALUE / 2
            val initialization = s"${parameter.name} = values.get($indexSoFar) > $halfMaxValue ? true : false;"
            val print = s"""System.out.println("${parameter.name}: " + ${parameter.name});"""
            (indexSoFar + 1,
              declaration :: declarations,
              initialization :: initializations,
              print :: prints)
          case _ => throw new Exception(s"Not support a parameter of type ${parameter.typ}")
        }
    })
    (declarations.reverse, initializations.reverse, prints.reverse)
  }
}
