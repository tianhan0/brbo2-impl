package brbo.benchmarks.sas22.test;

import brbo.benchmarks.Common;

public abstract class AmortizeSeparately extends Common {
    void main(int[] array1, int[] array2) {
        upperBound(0, "most", arraySum(array1) + arraySum(array2));
        int i = 0;
        while (i < arrayLength(array1)) {
            use(0, arrayRead(array1, i));
            use(0, arrayRead(array2, i));
            i++;
        }
        // Separately amortize the two arrays
    }
}
