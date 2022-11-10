package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class NonTermination extends Common {
    void main(int costA, int costB) {
        // upperBound(0, "most", costA + costB);
        while (true) {
            use(0, costA);
            use(0, costB);
            use(0, -costA);
            use(0, -costB);
        }
        // Verify bounds on a non-terminating program
    }
}
