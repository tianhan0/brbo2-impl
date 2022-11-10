package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class LoopPhase02 extends Common {
    void main(int costA, int costB) {
        // upperBound(0, "most", 50 * costA + 50 * costB);
        int x = 0;
        int y = 0;
        while (x < 100) {
            x = x + 1;
            if (x >= 50) {
                y = y + 1;
                use(0, costB);
            } else {
                use(0, costA);
            }
        }
        // There is a single path to decompose
    }
}
