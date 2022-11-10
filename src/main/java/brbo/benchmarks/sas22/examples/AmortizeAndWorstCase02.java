package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase02 extends Common {
    void main(int[] array, int costA, int costB, int n) {
        // upperBound(0, "most", arraySum(array) + costA * arraySum(array));
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < arrayLength(array)) {
                use(0, arrayRead(array, j));
                use(0, costA);
                j++;
            }
            use(0, costB);
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second and third use
    }
}
