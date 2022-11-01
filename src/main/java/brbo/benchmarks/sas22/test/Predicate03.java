package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class Predicate03 extends Common {
    void main(int costA, int costB, boolean decision, int n) {
        upperBound(0, "most", (costA >= costB ? costA : costB) * n);
        int i = 0;
        while (i < n) {
            int e = 0;
            if (decision) {
                e = costA;
            } else {
                e = costB;
            }
            // Need a predicate here to distinguish the 2 branches
            use(0, e);
            i++;
        }
    }
}
