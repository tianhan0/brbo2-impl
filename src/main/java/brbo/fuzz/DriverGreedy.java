package brbo.fuzz;

import edu.cmu.sv.kelinci.Kelinci;
import edu.cmu.sv.kelinci.Mem;
import edu.cmu.sv.kelinci.quantification.Greedy;
import edu.cmu.sv.kelinci.quantification.PartitionAlgorithm;
import edu.cmu.sv.kelinci.quantification.PartitionSet;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DriverGreedy {
  public final static int MAX_ARRAY_SIZE = 16;
  private final static short MAX_INTEGER = 30;
  private final static short MIN_INTEGER = 1;
  private final static int MAX_NUMBER_OF_USES_TO_TRACK = 1000;

  /* Minimum distance between clusters. */
  public final static double epsilon = 1.0;

  /* Cluster Algorithm */
  public static PartitionAlgorithm clusterAlgorithm = new Greedy(false);

  public static void main(String[] args) {
    if (args.length != 1) {
      System.out.println("Expects file name as parameter");
      return;
    }

    List<Short> values = new ArrayList<>();
    try (FileInputStream inputStream = new FileInputStream(args[0])) {
      System.out.printf("Reading at most %d shorts that are between [%d, %d]\n",
          MAX_ARRAY_SIZE, MIN_INTEGER, MAX_INTEGER);
      byte[] bytes = new byte[Short.BYTES];
      while ((inputStream.read(bytes) != -1) && (values.size() < MAX_ARRAY_SIZE)) {
        short value = ByteBuffer.wrap(bytes).getShort();
        value = value >= 0 ? value : (short) -value;
        value = (short) (value % (MAX_INTEGER - MIN_INTEGER + 1) + MIN_INTEGER);
        System.out.println("Read value: " + value);
        values.add(value);
      }
    } catch (IOException e) {
      System.err.println("Error reading input");
      e.printStackTrace();
      return;
    }

    int[] public_input = new int[values.size()];

    for (int i = 0; i < values.size() - 1; i++) {
      public_input[i] = values.get(i);
    }
    int public_input_2 = values.get(values.size() - 1);

    System.out.println("public: " + Arrays.toString(public_input));

    long[] observations = new long[MAX_NUMBER_OF_USES_TO_TRACK];
    Mem.clear(true);
    for (int i = 0; i < MAX_NUMBER_OF_USES_TO_TRACK; i++) {
      // In the i-th iteration, we collect accumulated resource consumption up to the secret[i]-th uses
      Mem.clear(false);
      // Resource.run(public_input, secrets[i]);
      Resource.run3(public_input, public_input_2, i);
      if (i == 0) {
        observations[i] = Mem.instrCost;
      } else {
        observations[i] = Mem.instrCost - observations[i - 1];
      }
    }
    System.out.println("observations: " + Arrays.toString(observations));

    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, observations, clusterAlgorithm);
    // Give feedback to fuzzer: Number of clusters, min distance between clusters
    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());

    System.out.println("Done.");
  }
}