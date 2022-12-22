package brbo.fuzz.blazer_login_unsafe.src;

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

public class Driver_Greedy {

  /* Maximum number of different observations. */
  public final static int K = 16;

  /* Minimum distance between clusters. */
  public final static double epsilon = 1.0;

  /* Cluster Algorithm */
  public static PartitionAlgorithm clusterAlgorithm = new Greedy(false);

  public static void main(String[] args) {

    if (args.length != 1) {
      System.out.println("Expects file name as parameter");
      return;
    }

    int[] public_input = new int[K];
    int[][] secret_taints = new int[K][1];

    List<Integer> values = new ArrayList<>();

    /* Read all values. */
    try (FileInputStream fis = new FileInputStream(args[0])) {
      byte[] bytes = new byte[Integer.BYTES];
      while ((fis.read(bytes) != -1) && (values.size() < K)) {
        values.add(ByteBuffer.wrap(bytes).getInt());
      }
    } catch (IOException e) {
      System.err.println("Error reading input");
      e.printStackTrace();
      return;
    }
    if (values.size() < K) {
      throw new RuntimeException("Too less data!");
    }

    /* Parse public values. */
    for (int i = 0; i < K; i++) {
      public_input[i] = values.get(i);
    }
    // public_input is [2,3,4]

    for (int i = 0; i < K; i++) {
      secret_taints[i][0] = public_input[i];
    }
    // secret_taints[0] = [2]
    // secret_taints[1] = [3]
    // secret_taints[2] = [4]

    // secret_taints[0] = 1 means collecting the 1st resource update
    // secret_taints[1] = 2 means collecting the 2nd

    // System.out.println("public=" + public_input);
    for (int i = 0; i < secret_taints.length; i++) {
      System.out.println("secret" + i + "=" + secret_taints[i]);
    }

    long[] observations = new long[K];
    Mem.clear(true);
    for (int i = 0; i < K; i++) {
      Mem.clear(false);
      Resource.run(public_input, i);
      observations[i] = Mem.instrCost;
    }
    System.out.println("observations: " + Arrays.toString(observations));

    PartitionSet clusters = PartitionSet.createFromObservations(epsilon, observations, clusterAlgorithm);
    // Give feedback to fuzzer: Number of clusters, min distance between clusters
    Kelinci.setObserverdClusters(clusters.getClusterAverageValues(), clusters.getMinimumDeltaValue());

    System.out.println("Done.");
  }
}
