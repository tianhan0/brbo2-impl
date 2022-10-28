package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class LoopPhase01 extends Common {
    void main(int costA, int costB, int n) {
        upperBound(0, "most", costA + (n - 1) * costB);
        int i = 0;
        while (i < n) {
            if (i == 0) {
                use(0, costA);
            } else {
                use(0, costB);
            }
            i++;
        }
        // There is a single path to decompose
    }
}
