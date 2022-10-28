package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class PathPermutation extends Common {
    void main(int costA, int costB, int n) {
        upperBound(0, "most", n * (costA >= costB ? costA : costB));
        int i = 0;
        while (i < n) {
            if (ndInt() > 0) {
                use(0, costA);
            } else {
                use(0, costB);
            }
            i++;
        }
        // Any path is the permutations of another path
    }
}
