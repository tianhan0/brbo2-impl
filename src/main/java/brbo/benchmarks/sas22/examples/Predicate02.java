package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class Predicate02 extends Common {
    void main(int costA, int costB, int costC) {
        // upperBound(0, "most", (costA >= costB ? costA : costB) + 2 * costA);
        if (ndInt() > 0) {
            use(0, costB);
        } else {
            use(0, costC);
        }
        // Not need a predicate here to express the decompositions for the 2 paths,
        // because they can be decomposed in the same way
        use(0, costA);
        use(0, costA);
    }
}
