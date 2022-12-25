package brbo.backend2.qfuzz

import brbo.common.BrboType
import brbo.common.BrboType.QFuzzPrintType
import brbo.common.ast.{BrboProgram, Identifier}
import brbo.common.string.StringFormatUtils
import brbo.frontend.TargetProgram

object DriverGenerator {
  private val ARRAY_SIZE = 8;
  private val MAX_INTEGER = 30;
  private val MIN_INTEGER = 1;
  private val MAX_NUMBER_OF_USES_TO_TRACK = 1000;

  def run(program: BrboProgram): String = {
    val (declarations, initializations) = declarationsAndInitializations(program.mainFunction.parameters)
    val transformedProgram = ProgramTransformer.transform(program)
    s"""${transformedProgram.printToQFuzzJava(indent = 0)}
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
       |public class ${driverName(program.className)} {
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
       |  public static void main(String[] args) {
       |    if (args.length != 1) {
       |      System.out.println("Expects file name as parameter");
       |      return;
       |    }
       |
       |    List<Short> values = new ArrayList<>();
       |    try (FileInputStream inputStream = new FileInputStream(args[0])) {
       |      System.out.printf("Reading shorts that are between [%d, %d]", MIN_INTEGER, MAX_INTEGER);
       |      byte[] bytes = new byte[Short.BYTES];
       |      while ((inputStream.read(bytes) != -1)) {
       |        short value = ByteBuffer.wrap(bytes).getShort();
       |        value = value >= 0 ? value : (short) -value;
       |        value = (short) (value % (MAX_INTEGER - MIN_INTEGER + 1) + MIN_INTEGER);
       |        System.out.println("Read value: " + value);
       |        values.add(value);
       |      }
       |    } catch (IOException e) {
       |      System.err.println("Error reading input");
       |      e.printStackTrace();
       |      return;
       |    }
       |
       |${prependIndents(declarations, indent = 4).mkString("\n")}
       |${prependIndents(initializations, indent = 4).mkString("\n")}
       |
       |    System.out.println("public: " + Arrays.toString(public_input));
       |
       |    long[] observations = new long[MAX_NUMBER_OF_USES_TO_TRACK];
       |    ${program.className} program = new ${program.className}();
       |    Mem.clear(true);
       |    for (int i_th_use = 0; i_th_use < MAX_NUMBER_OF_USES_TO_TRACK; i_th_use++) {
       |      // In the i-th iteration, we collect accumulated resource consumption up to the secret[i]-th uses
       |      Mem.clear(false);
       |      program.${TargetProgram.MAIN_FUNCTION}(${program.mainFunction.parameters.map({ identifier => identifier.name }).mkString(", ")}, i_th_use);
       |      if (i_th_use == 0) {
       |        observations[i_th_use] = Mem.instrCost;
       |      } else {
       |        observations[i_th_use] = Mem.instrCost - observations[i_th_use - 1];
       |      }
       |    }
       |    System.out.println("observations: " + Arrays.toString(observations));
       |
       |    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, observations, clusterAlgorithm);
       |    // Give feedback to fuzzer: Number of clusters, min distance between clusters
       |    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());
       |
       |    System.out.println("Done.");
       |  }
       |}""".stripMargin
  }

  def driverName(className: String): String = s"${className}Driver"

  def prependIndents(lines: List[String], indent: Int): List[String] =
    lines.map(line => StringFormatUtils.prependIndentsPerLine(line, indent))

  def declarationsAndInitializations(parameters: List[Identifier]): (List[String], List[String]) = {
    val (_, declarations, initializations) = parameters.foldLeft((0, Nil: List[String], Nil: List[String]))({
      case ((indexSoFar, declarations, initializations), parameter) =>
        parameter.typ match {
          case BrboType.INT =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)} = values.get($indexSoFar);"
            (indexSoFar + 1, declaration :: declarations, initializations)
          case BrboType.ARRAY(BrboType.INT) =>
            val declaration = s"${parameter.typeNamePair(QFuzzPrintType)} = new int[ARRAY_SIZE];"
            val initialization =
              s"""for (int i = 0; i < ARRAY_SIZE; i++) {
                 |  ${parameter.name}[i] = values.get($indexSoFar + i);
                 |}""".stripMargin
            (indexSoFar + ARRAY_SIZE,
              declaration :: declarations,
              initialization :: initializations)
          case _ => throw new Exception
        }
    })
    (declarations.reverse, initializations.reverse)
  }
}
