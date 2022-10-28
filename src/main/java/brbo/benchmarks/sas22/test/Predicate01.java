package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class Predicate01 extends Common {
    void main(int costA, int costB, int n) {
        upperBound(0, "most", (costA >= costB ? costA : costB) + 2 * costA);
        if (ndInt() > 0) {
            use(0, costA);
        } else {
            use(0, costB);
        }
        // Need a predicate here to express 2 decompositions as a single program
        use(0, costA);
        use(0, costA);
    }
}
