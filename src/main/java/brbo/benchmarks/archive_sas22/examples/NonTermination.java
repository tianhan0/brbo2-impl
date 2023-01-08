package brbo.benchmarks.archive_sas22.examples;

import brbo.benchmarks.Common;

public abstract class NonTermination extends Common {
    void main(int costA, int costB) {
        int R = 0;
        mostPreciseBound(R <= costA + costB);
        lessPreciseBound(R <= MAX * costA + MAX * costB);
        while (true) {
            R = R + costA;
            R = R + costB;
            R = R - costA;
            R = R - costB;
        }
        // Verify bounds on a non-terminating program
    }
}
