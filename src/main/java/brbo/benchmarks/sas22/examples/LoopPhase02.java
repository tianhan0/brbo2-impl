package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class LoopPhase02 extends Common {
    void main(int costA, int costB, int n) {
        int x = 0;
        int y = 0;
        int R = 0;
        mostPreciseBound(R <= n * costA || R <= (n - 25) * costB + 25 * costA);
        lessPreciseBound(R <= MAX * n * costA || R <= MAX * (n - 25) * costB + MAX * 25 * costA);
        int e = 0;
        while (x < n) {
            x = x + 1;
            if (x >= 2) {
                y = y + 1;
                e = costB;
            } else {
                e = costA;
            }
            R = R + e;
        }
        // There is a single path to decompose
    }
}
