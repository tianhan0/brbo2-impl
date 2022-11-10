package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase01 extends Common {
    /*void main(int[] array, int costA) {
        // upperBound(0, "most", arraySum(array) + costA * arraySum(array));
        int i = 0;
        while (i < arrayLength(array)) {
            use(0, arrayRead(array, i));
            use(0, costA);
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second use
    }*/

    void main(int array, int costA) {
        // upperBound(0, "most", arraySum(array) + costA * arraySum(array));
        int i = 0;
        int R = 0;
        for (int it0 = array, entry0 = ndInt2(1, it0); it0 > 0; it0 -= entry0, entry0 = ndInt2(1, it0)) {
            R = R + entry0;
            R = R + costA;
        }
        // Amortize the first use and apply the worst-case reasoning on the second use
    }
}
