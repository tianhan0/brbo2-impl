package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class LoopPhase01 extends Common {
    void main(int costA, int costB, int n) {
        int i = 0;
        int R = 0;
        mostPreciseBound(R <= costA + (n - 1) * costB);
        lessPreciseBound(R <= MAX * costA + MAX * (n - 1) * costB);
        int e = 0;
        while (i < n) {
            if (i == 0) {
                e = costA;
            } else {
                e = costB;
            }
            R = R + e;
            i++;
        }
        // There is a single path to decompose
    }
}
