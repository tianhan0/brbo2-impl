package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeSeparately02 extends Common {
    void main(int[] array, int costA) {
        // upperBound(0, "most", arraySum(array) + costA * arraySum(array));
        int i = 0;
        while (i < arrayLength(array)) {
            use(0, arrayRead(array, i));
            use(0, costA);
            i++;
        }
        i = 0;
        while (i < arrayLength(array)) {
            use(0, arrayRead(array, i));
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second use
        // Need to separately amortize the first and the third use
    }
}
