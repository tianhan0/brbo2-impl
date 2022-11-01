package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase01 extends Common {
    void main(int[] array, int costA) {
        upperBound(0, "most", arraySum(array) + costA * arraySum(array));
        int i = 0;
        while (i < arrayLength(array)) {
            use(0, arrayRead(array, i));
            use(0, costA);
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second use
    }
}
