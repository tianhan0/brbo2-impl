package brbo.benchmarks.archive_sas22.examples;

import brbo.benchmarks.Common;

public abstract class MultiPath01 extends Common {
    void main(int[] text, int costA, int decision) {
        // upperBound(0, "most", arraySum(text) >= costA ? arraySum(text) : costA);
        if (decision > 100) {
            int i = 0;
            while (i < arrayLength(text)) {
                use(0, arrayRead(text, i));
                i++;
            }
        } else {
            use(0, costA);
        }
        // 2 different paths
    }
}
