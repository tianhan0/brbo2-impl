package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase02Array extends Common {
    void main(int[] array, int costA, int costB, int n) {
        int i = 0;
        int R = 0;
        int j = 0;
        int k = 0;
        int e = 0;
        mostPreciseBound(R <= n * costB + n * arraySum(array) * costA + n * arraySum(array));
        lessPreciseBound(R <= MAX * n * costB + MAX * n * arraySum(array) * costA + MAX * n * arraySum(array));
        while (i < n) {
            k = 0;
            while (k < arrayLength(array)) {
                e = arrayRead(array, k);
                R = R + e;
                R = R + costA;
                k++;
                j++;
            }
            R = R + costB;
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second and third use
    }
}
