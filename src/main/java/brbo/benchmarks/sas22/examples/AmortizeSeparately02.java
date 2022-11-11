package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeSeparately02 extends Common {
    void main(int array, int costA) {
        int it = array;
        int e = 0;
        int R = 0;
        while (it > 0) {
            e = ndInt2(1, it);
            it -= e;
            R = R + e;
            R = R + costA;
        }
        it = array;
        while (it > 0) {
            e = ndInt2(1, it);
            it -= e;
            R = R + e;
        }
        // Amortize the first use and apply the worst-case reasoning on the second use
        // Need to separately amortize the first and the third use
    }
}
