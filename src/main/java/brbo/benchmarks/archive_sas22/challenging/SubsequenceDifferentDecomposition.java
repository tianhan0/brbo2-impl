package brbo.benchmarks.archive_sas22.challenging;

import brbo.benchmarks.Common;

public abstract class SubsequenceDifferentDecomposition extends Common {
    void main(int costA, int costB, int costC, int n) {
        // upperBound(0, "most", (costA >= costB ? costA : costB) + 2 * costA);
        int e = costB;
        if (n > 0)
            e = costA;
        use(0, e);

        e = costC;
        if (n > 0)
            e = costA;
        use(0, e);
        // Although path with costs B, C is a subsequence of path with costs A, A,
        // the two paths need to be decomposed differently
    }
}
