package brbo.benchmarks.sas22.examples;

import brbo.benchmarks.Common;

public abstract class AmortizeAndWorstCase02 extends Common {
    void main(int array, int costA, int costB, int n) {
        int i = 0;
        int it = 0;
        int e = 0;
        int R = 0;
        mostPreciseBound(R <= n * costB + n * array * costA + n * array);
        lessPreciseBound(R <= MAX * n * costB + MAX * n * array * costA + MAX * n * array);
        while (i < n) {
            it = array;
            while (it > 0) {
                e = ndInt2(1, it);
                R = R + e;
                R = R + costA;
                it -= e;
            }
            use(0, costB);
            i++;
        }
        // Amortize the first use and apply the worst-case reasoning on the second and third use
    }
}
